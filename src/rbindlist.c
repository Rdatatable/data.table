#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <Rversion.h>
#include <stdint.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee
// raise(SIGINT);

extern size_t sizes[100];
#define SIZEOF(x) sizes[TYPEOF(x)]

/* Eddi's hash setup for combining factor levels appropriately - untouched from previous state (except made combineFactorLevels static) */

// Fixes #5150
// a simple check for R version to decide if the type should be R_len_t or R_xlen_t
// long vector support was added in R 3.0.0
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 0, 0)
  typedef R_xlen_t RLEN;
#else
  typedef R_len_t RLEN;
#endif

extern int StrCmp(SEXP x, SEXP y);    // in forder.c

// a simple linked list, will use this when finding global order for ordered factors
// will keep two ints
struct llist {
    struct llist * next;
    R_len_t i, j;
};

// hash table code copied from main/unique.c, specialized for our particular needs
// as our table will just be strings
// took out long vector ifdefs as that relied on too much base code
// can revisit this later if there is need for more than ~1e9 length factor columns
// UTF8 and Cache bools are not set correctly for now

typedef size_t hlen;

/* Hash function and equality test for keys */
typedef struct _HashData HashData;

struct _HashData {
    int K;
    hlen M;
    RLEN nmax;
    hlen (*hash)(SEXP, RLEN, HashData *);
    int (*equal)(SEXP, RLEN, SEXP, RLEN);
    struct llist ** HashTable;

    int nomatch;
    Rboolean useUTF8;
    Rboolean useCache;
};

/*
Integer keys are hashed via a random number generator
based on Knuth's recommendations. The high order K bits
are used as the hash code.

NB: lots of this code relies on M being a power of two and
on silent integer overflow mod 2^32.

<FIXME> Integer keys are wasteful for logical and raw vectors, but
the tables are small in that case. It would be much easier to
implement long vectors, though.
*/

/* Currently the hash table is implemented as a (signed) integer
array. So there are two 31-bit restrictions, the length of the
array and the values. The values are initially NIL (-1). O-based
indices are inserted by isDuplicated, and invalidated by setting
to NA_INTEGER.
*/

static hlen scatter(unsigned int key, HashData *d)
{
    return 3141592653U * key >> (32 - d->K);
}

/* Hash CHARSXP by address. Hash values are int, For 64bit pointers,
 * we do (upper ^ lower) */
static hlen cshash(SEXP x, RLEN indx, HashData *d)
{
    intptr_t z = (intptr_t) STRING_ELT(x, indx);
    unsigned int z1 = (unsigned int)(z & 0xffffffff), z2 = 0;
#if SIZEOF_LONG == 8
    z2 = (unsigned int)(z/0x100000000L);
#endif
    return scatter(z1 ^ z2, d);
}

static hlen shash(SEXP x, RLEN indx, HashData *d)
{
    unsigned int k;
    const char *p;
    const void *vmax = vmaxget();
    if(!d->useUTF8 && d->useCache) return cshash(x, indx, d);
    /* Not having d->useCache really should not happen anymore. */
    p = translateCharUTF8(STRING_ELT(x, indx));
    k = 0;
    while (*p++)
        k = 11 * k + (unsigned int) *p; /* was 8 but 11 isn't a power of 2 */
    vmaxset(vmax); /* discard any memory used by translateChar */
    return scatter(k, d);
}

static int sequal(SEXP x, RLEN i, SEXP y, RLEN j)
{
    // using our function instead of copying a lot more code from base
    return !StrCmp(STRING_ELT(x, i), STRING_ELT(y, j));
}

/*
Choose M to be the smallest power of 2
not less than 2*n and set K = log2(M).
Need K >= 1 and hence M >= 2, and 2^M < 2^31-1, hence n <= 2^29.

Dec 2004: modified from 4*n to 2*n, since in the worst case we have
a 50% full table, and that is still rather efficient -- see
R. Sedgewick (1998) Algorithms in C++ 3rd edition p.606.
*/
static void MKsetup(HashData *d, RLEN n)
{
    if(n < 0 || n >= 1073741824) /* protect against overflow to -ve */
        error("length %d is too large for hashing", n);

    size_t n2 = 2U * (size_t) n;
    d->M = 2;
    d->K = 1;
    while (d->M < n2) {
        d->M *= 2;
        d->K++;
    }
    d->nmax = n;
}

#define IMAX 4294967296L
static void HashTableSetup(HashData *d, RLEN n)
{
    d->hash = shash;
    d->equal = sequal;
    MKsetup(d, n);
    d->HashTable = malloc(sizeof(struct llist *) * (d->M));
    if (d->HashTable == NULL) error("malloc failed in rbindlist.c. This part of the code will be reworked.");
    for (RLEN i = 0; i < d->M; i++) d->HashTable[i] = NULL;
}

static void CleanHashTable(HashData *d)
{
    struct llist * root, * tmp;

    for (RLEN i = 0; i < d->M; ++i) {
        root = d->HashTable[i];
        while (root != NULL) {
            tmp = root->next;
            free(root);
            root = tmp;
        }
    }
    free(d->HashTable);
}

// factorType is 1 for factor and 2 for ordered
// will simply unique normal factors and attempt to find global order for ordered ones
static SEXP combineFactorLevels(SEXP factorLevels, int * factorType, Rboolean * isRowOrdered) {
    // find total length
    RLEN size = 0;
    R_len_t len = LENGTH(factorLevels), n, i, j;
    SEXP elem;
    for (i = 0; i < len; ++i) {
        elem = VECTOR_ELT(factorLevels, i);
        n = LENGTH(elem);
        size += n;
        /* for (j = 0; j < n; ++j) { */
        /*     if(IS_BYTES(STRING_ELT(elem, j))) { */
        /*         data.useUTF8 = FALSE; break; */
        /*     } */
        /*     if(ENC_KNOWN(STRING_ELT(elem, j))) { */
        /*         data.useUTF8 = TRUE; */
        /*     } */
        /*     if(!IS_CACHED(STRING_ELT(elem, j))) { */
        /*         data.useCache = FALSE; break; */
        /*     } */
        /* } */
    }

    // set up hash to put duplicates in
    HashData data;
    data.useUTF8 = FALSE;
    data.useCache = TRUE;
    HashTableSetup(&data, size);

    struct llist **h = data.HashTable;
    hlen idx;
    struct llist * pl;
    R_len_t uniqlen = 0;
    // we insert in opposite order because it's more convenient later to choose first of the duplicates
    for (i = len-1; i >= 0; --i) {
        elem = VECTOR_ELT(factorLevels, i);
        n = LENGTH(elem);
        for (j = n-1; j >= 0; --j) {
            idx = data.hash(elem, j, &data);
            while (h[idx] != NULL) {
                pl = h[idx];
                if (data.equal(VECTOR_ELT(factorLevels, pl->i), pl->j, elem, j))
                    break;
                // it's a collision, not a match, so iterate to a new spot
                idx = (idx + 1) % data.M;
            }
            if (data.nmax-- < 0) error("hash table is full");

            pl = malloc(sizeof(struct llist));
            if (pl == NULL) error("malloc failed in rbindlist.c. This part of the code will be reworked.");
            pl->next = NULL;
            pl->i = i;
            pl->j = j;
            if (h[idx] != NULL) {
                pl->next = h[idx];
            } else {
                ++uniqlen;
            }
            h[idx] = pl;
        }
    }

    SEXP finalLevels = PROTECT(allocVector(STRSXP, uniqlen));
    R_len_t counter = 0;
    if (*factorType == 2) {
        int * locs = malloc(sizeof(int) * len);
        if (locs == NULL) error("malloc failed in rbindlist.c. This part of the code will be reworked.");
        for (i = 0; i < len; ++i) locs[i] = 0;

        R_len_t k;
        SEXP tmp;
        for (i = 0; i < len; ++i) {
            if (!isRowOrdered[i]) continue;

            elem = VECTOR_ELT(factorLevels, i);
            n = LENGTH(elem);
            for (j = locs[i]; j < n; ++j) {
                idx = data.hash(elem, j, &data);
                while (h[idx] != NULL) {
                    pl = h[idx];
                    if (data.equal(VECTOR_ELT(factorLevels, pl->i), pl->j, elem, j)) {
                        do {
                            if (!isRowOrdered[pl->i]) continue;

                            tmp = VECTOR_ELT(factorLevels, pl->i);
                            if (locs[pl->i] > pl->j) {
                                // failed to construct global order, need to break out of too many loops
                                // so will use goto :o
                                warning("ordered factor levels cannot be combined, going to convert to simple factor instead");
                                counter = 0;
                                *factorType = 1;
                                goto normalFactor;
                            }

                            for (k = locs[pl->i]; k < pl->j; ++k) {
                                SET_STRING_ELT(finalLevels, counter++, STRING_ELT(tmp, k));
                            }
                            locs[pl->i] = pl->j + 1;
                        } while ( (pl = pl->next) ); // added parenthesis to remove compiler warning 'suggest parentheses around assignment used as truth value'
                        SET_STRING_ELT(finalLevels, counter++, STRING_ELT(elem, j));
                        break;
                    }
                    // it's a collision, not a match, so iterate to a new spot
                    idx = (idx + 1) % data.M;
                }
                if (h[idx] == NULL) error("internal hash error, please report to datatable-help");
            }
        }

        // fill in the rest of the unordered elements
        Rboolean record;
        for (i = 0; i < len; ++i) {
            if (isRowOrdered[i]) continue;

            elem = VECTOR_ELT(factorLevels, i);
            n = LENGTH(elem);
            for (j = 0; j < n; ++j) {
                idx = data.hash(elem, j, &data);
                while (h[idx] != NULL) {
                    pl = h[idx];
                    if (data.equal(VECTOR_ELT(factorLevels, pl->i), pl->j, elem, j)) {
                        record = TRUE;
                        do {
                            // if this element was in an ordered list, it's been recorded already
                            if (isRowOrdered[pl->i]) {
                                record = FALSE;
                                break;
                            }
                        } while ( (pl = pl->next) ); // added parenthesis to remove compiler warning 'suggest parentheses around assignment used as truth value'
                        if (record)
                            SET_STRING_ELT(finalLevels, counter++, STRING_ELT(elem, j));

                        break;
                    }
                    // it's a collision, not a match, so iterate to a new spot
                    idx = (idx + 1) % data.M;
                }
                if (h[idx] == NULL) error("internal hash error, please report to datatable-help");
            }
        }
        free (locs);
    }

 normalFactor:
    if (*factorType == 1) {
        for (i = 0; i < len; ++i) {
            elem = VECTOR_ELT(factorLevels, i);
            n = LENGTH(elem);
            for (j = 0; j < n; ++j) {
                idx = data.hash(elem, j, &data);
                while (h[idx] != NULL) {
                    pl = h[idx];
                    if (data.equal(VECTOR_ELT(factorLevels, pl->i), pl->j, elem, j)) {
                        if (pl->i == i && pl->j == j) {
                            SET_STRING_ELT(finalLevels, counter++, STRING_ELT(elem, j));
                        }
                        break;
                    }
                    // it's a collision, not a match, so iterate to a new spot
                    idx = (idx + 1) % data.M;
                }
                if (h[idx] == NULL) error("internal hash error, please report to datatable-help");
            }
        }
    }

    CleanHashTable(&data);

    return finalLevels;
}


/* Arun's addition and changes to incorporate 'usenames=T/F' and 'fill=T/F' arguments to rbindlist */

extern SEXP forder(SEXP DT, SEXP by, SEXP retGrp, SEXP sortStrArg, SEXP orderArg, SEXP naArg);
extern SEXP allocNAVector(SEXPTYPE type, R_len_t n);

/*
    l               = input list of data.tables/lists/data.frames
    n               = length(l)
    ans             = rbind'd result
    i               = an index over length of l
    use.names       = whether binding should check for names and bind them accordingly
    fill            = whether missing columns should be filled with NAs

    ans_ptr         = final_names - column names for 'ans' (list item 1)
    ans_ptr         = match_indices - when use.names=TRUE, for each element in l, what's the destination col index (in 'ans') for each of the cols (list item 2)
    n_rows          = total number of rows in 'ans'
    n_cols          = total number of cols in 'ans'
    max_type        = for each col in 'ans', what's the final SEXPTYPE? (for coercion if necessary)
    is_factor       = for each col in 'ans' mark which one's a factor (to convert to factor at the end)
    is_ofactor      = for each col in 'ans' mark which one's an ordered factor (to convert to ordered factor at the end)
    fn_rows         = the length of first column (rows) for each item in l.
    mincol          = get the minimum number of columns in an item from l. Used to check if 'fill=TRUE' is really necessary, even if set.
*/

struct preprocessData {
    SEXP ans_ptr;
    SEXP colname;
    size_t n_rows;
    size_t n_cols;
    int *fn_rows;
    SEXPTYPE *max_type;
    int *is_factor;
    int first;
    int lcount;
    int mincol;
};

static SEXP unlist2(SEXP v) {
    
    RLEN i, j, k=0, ni, n=0;
    SEXP ans, vi, lnames, groups, runids;

    for (i=0; i<length(v); i++) n += length(VECTOR_ELT(v, i));
    ans    = PROTECT(allocVector(VECSXP, 3));
    lnames = PROTECT(allocVector(STRSXP, n));
    groups = PROTECT(allocVector(INTSXP, n));
    runids = PROTECT(allocVector(INTSXP, n));
    for (i=0; i<length(v); i++) {
        vi = VECTOR_ELT(v, i);
        ni = length(vi);
        for (j=0; j<ni; j++) {
            SET_STRING_ELT(lnames, k + j, STRING_ELT(vi, j));
            INTEGER(groups)[k + j] = i+1;
            INTEGER(runids)[k + j] = j;
        }
        k+=j;
    }
    SET_VECTOR_ELT(ans, 0, lnames);
    SET_VECTOR_ELT(ans, 1, groups);
    SET_VECTOR_ELT(ans, 2, runids);
    UNPROTECT(4);
    return(ans);
}

static SEXP fast_order(SEXP dt, R_len_t byArg) {

    R_len_t i, protecti=0;
    SEXP ans, by, retGrp, sortStr, order, na, starts;

    by      = PROTECT(allocVector(INTSXP, byArg));
    retGrp  = PROTECT(allocVector(LGLSXP, 1)); 
    sortStr = PROTECT(allocVector(LGLSXP, 1)); 
    order   = PROTECT(allocVector(INTSXP, byArg));
    na      = PROTECT(allocVector(LGLSXP, 1));

    LOGICAL(retGrp)[0] = TRUE; LOGICAL(sortStr)[0] = FALSE; LOGICAL(na)[0] = FALSE;
    for (i=0; i<byArg; i++) {
        INTEGER(by)[i] = i+1;
        INTEGER(order)[i] = 1;
    }
    UNPROTECT(5);

    ans = PROTECT(forder(dt, by, retGrp, sortStr, order, na)); protecti++;
    if (!length(ans)) {
        starts = PROTECT(getAttrib(ans, mkString("starts"))); protecti++;
        // if cols are already sorted, 'forder' gives integer(0), got to replace it with 1:.N
        ans = PROTECT(allocVector(INTSXP, length(VECTOR_ELT(dt, 0)))); protecti++;
        for (i=0; i<length(ans); i++) INTEGER(ans)[i] = i+1;
        setAttrib(ans, install("starts"), starts);
    }
    UNPROTECT(protecti); // ans
    return(ans);
}

static SEXP uniq_lengths(SEXP v, R_len_t n) {
    
    R_len_t i, nv=length(v);
    SEXP ans = PROTECT(allocVector(INTSXP, nv));
    for (i=1; i<nv; i++) {
        INTEGER(ans)[i-1] = INTEGER(v)[i] - INTEGER(v)[i-1];
    }
    // last value
    INTEGER(ans)[nv-1] = n - INTEGER(v)[nv-1] + 1;
    UNPROTECT(1);
    return(ans);
}

static SEXP match_names(SEXP v) {
    
    R_len_t i, j, k, idx, ncols, protecti=0;
    SEXP ans, dt, lnames, ti;
    SEXP uorder, starts, ulens, index;
    SEXP fnames, findices, runid, grpid;
    
    ans    = PROTECT(allocVector(VECSXP, 2));
    dt     = PROTECT(unlist2(v)); protecti++;
    lnames = VECTOR_ELT(dt, 0);
    grpid  = PROTECT(duplicate(VECTOR_ELT(dt, 1))); protecti++; // dt[1] will be reused, so backup
    runid  = VECTOR_ELT(dt, 2);
    
    uorder = PROTECT(fast_order(dt, 2));  protecti++; // byArg alone is set, everything else is set inside fast_order
    starts = PROTECT(getAttrib(uorder, mkString("starts"))); protecti++;
    ulens  = PROTECT(uniq_lengths(starts, length(lnames))); protecti++;
    
    // seq_len(.N) for each group
    index = PROTECT(VECTOR_ELT(dt, 1)); protecti++; // reuse dt[1] (in 0-index coordinate), value already backed up above.
    k=0;
    for (i=0; i<length(ulens); i++) {
        for (j=0; j<INTEGER(ulens)[i]; j++) {
            INTEGER(index)[INTEGER(uorder)[k+j]-1] = j;
        }
        k += j;
    }
    // order again
    uorder = PROTECT(fast_order(dt, 2));  protecti++; // byArg alone is set, everything else is set inside fast_order
    starts = PROTECT(getAttrib(uorder, mkString("starts"))); protecti++;
    ulens  = PROTECT(uniq_lengths(starts, length(lnames))); protecti++;    
    ncols  = length(starts);
    // get fnames and findices
    fnames   = PROTECT(allocVector(STRSXP, ncols)); protecti++;
    findices = PROTECT(allocVector(VECSXP, ncols)); protecti++;
    k=0;
    for (i=0; i<ncols; i++) {
        idx = INTEGER(uorder)[INTEGER(starts)[i]-1]-1;
        SET_STRING_ELT(fnames, i, STRING_ELT(lnames, idx));
        ti = PROTECT(allocVector(INTSXP, length(v)));
        for (j=0;j<length(v);j++) INTEGER(ti)[j]=-1; // TODO: can we eliminate this?
        for (j=0; j<INTEGER(ulens)[i]; j++) {
            idx = INTEGER(uorder)[k+j]-1;
            INTEGER(ti)[INTEGER(grpid)[idx]-1] = INTEGER(runid)[idx];
        }
        k+=j;
        UNPROTECT(1);
        SET_VECTOR_ELT(findices, i, ti);
    }
    UNPROTECT(protecti);
    SET_VECTOR_ELT(ans, 0, fnames);
    SET_VECTOR_ELT(ans, 1, findices);
    UNPROTECT(1); // ans
    return(ans);
}

static void preprocess(SEXP l, Rboolean usenames, Rboolean fill, struct preprocessData *data) {
    
    R_len_t i, j, idx;
    SEXP li, lnames=R_NilValue, fnames, findices=R_NilValue, f_ind=R_NilValue, thiscol, col_name=R_NilValue;
    SEXPTYPE type;
    
    data->first = -1; data->lcount = 0; data->n_rows = 0; data->n_cols = 0;
    data->max_type = NULL; data->is_factor = NULL; data->ans_ptr = R_NilValue; data->mincol=0;
    data->fn_rows = Calloc(length(l), int); data->colname = R_NilValue;

    // get first non null name, 'rbind' was doing a 'match.names' for each item.. which is a bit more time consuming.
    // And warning that it'll be matched by names is not necessary, I think, as that's the default for 'rbind'. We 
    // should instead document it.
    for (i=0; i<length(l); i++) { // length(l) = 0 is handled in rbindlist already.
        li = VECTOR_ELT(l, i);
        if (isNull(li)) continue;
        if (TYPEOF(li) != VECSXP) error("Item %d of list input is not a data.frame, data.table or list",i+1);
        if (!LENGTH(li)) continue;
        col_name = getAttrib(li, R_NamesSymbol);
        if (!isNull(col_name)) {
            data->colname = PROTECT(col_name);
            break;
        }
    }
    if (usenames) { lnames = PROTECT(allocVector(VECSXP, length(l)));}
    for (i=0; i<length(l); i++) {
        li = VECTOR_ELT(l, i);
        if (isNull(li)) continue;
        if (TYPEOF(li) != VECSXP) error("Item %d of list input is not a data.frame, data.table or list",i+1);
        if (!LENGTH(li)) continue;
        col_name = getAttrib(li, R_NamesSymbol);
        data->lcount++;
        data->fn_rows[i] = length(VECTOR_ELT(li, 0));
        if (data->first == -1) {
            data->first = i;
            data->n_cols = length(li);
            data->mincol = length(li);
            if (!usenames) {
                data->ans_ptr = PROTECT(allocVector(VECSXP, 2));
                if (isNull(col_name)) SET_VECTOR_ELT(data->ans_ptr, 0, data->colname);
                else SET_VECTOR_ELT(data->ans_ptr, 0, col_name);
            } else {
                if (isNull(col_name)) SET_VECTOR_ELT(lnames, i, data->colname);
                else SET_VECTOR_ELT(lnames, i, col_name);
            }
            data->n_rows += LENGTH(VECTOR_ELT(li,0));
            continue;
        } else {
            if (!fill && length(li) != data->n_cols)
                if (length(li) != data->n_cols) error("Item %d has %d columns, inconsistent with item %d which has %d columns. If instead you need to fill missing columns, use set argument 'fill' to TRUE.",i+1, length(li), data->first+1, data->n_cols);
        }
        if (data->mincol > length(li)) data->mincol = length(li);
        data->n_rows += length(VECTOR_ELT(li, 0));
        if (usenames) {
            if (isNull(col_name)) SET_VECTOR_ELT(lnames, i, data->colname);
            else SET_VECTOR_ELT(lnames, i, col_name);
        }
    }
    if (usenames) {
        data->ans_ptr = PROTECT(match_names(lnames));
        fnames = VECTOR_ELT(data->ans_ptr, 0);
        findices = VECTOR_ELT(data->ans_ptr, 1);
        if (!fill && length(fnames) != data->mincol) {
            error("Answer requires %d columns whereas one or more item(s) in the input list has only %d columns. This could be because the items in the list may not all have identical column names or some of the items may have duplicate names. In either case, if you're aware of this and would like to fill those missing columns, set the argument 'fill=TRUE'.", length(fnames), data->mincol);
        } else data->n_cols = length(fnames);
    }
    if (isNull(data->colname) && data->n_cols > 0)
        error("At least one item in the input list must have all column names set.\n");
    
    // decide type of each column
    // initialize the max types - will possibly increment later
    data->max_type  = Calloc(data->n_cols, SEXPTYPE);
    data->is_factor = Calloc(data->n_cols, int);
    for (i = 0; i< data->n_cols; i++) {
        if (usenames) f_ind = VECTOR_ELT(findices, i);
        for (j=data->first; j<length(l); j++) {
            if (data->is_factor[i] == 2) break;
            idx = (usenames) ? INTEGER(f_ind)[j] : i;
            li = VECTOR_ELT(l, j);
            if (isNull(li) || !LENGTH(li) || idx < 0) continue;
            thiscol = VECTOR_ELT(li, idx);
            if (isFactor(thiscol)) {
                data->is_factor[i] = (isOrdered(thiscol)) ? 2 : 1;
                data->max_type[i]  = STRSXP;
            } else {
                type = TYPEOF(thiscol);
                if (type > data->max_type[i]) data->max_type[i] = type;
            }
        }
    }
}

SEXP rbindlist(SEXP l, SEXP sexp_usenames, SEXP sexp_fill) {
    
    R_len_t jj, ansloc, resi, i,j,r, idx, thislen;
    struct preprocessData data; 
    Rboolean usenames, fill, to_copy = FALSE, coerced=FALSE;
    SEXP fnames = R_NilValue, findices = R_NilValue, f_ind = R_NilValue, ans, lf, li, target, thiscol, levels;
    SEXP factorLevels = R_NilValue, finalFactorLevels;
    Rboolean *isRowOrdered = NULL;
    R_len_t protecti;
    
    // first level of error checks
    if (!isLogical(sexp_usenames) || LENGTH(sexp_usenames)!=1 || LOGICAL(sexp_usenames)[0]==NA_LOGICAL)
        error("use.names should be TRUE or FALSE");
    if (!isLogical(sexp_fill) || LENGTH(sexp_fill) != 1 || LOGICAL(sexp_fill)[0] == NA_LOGICAL)
        error("fill should be TRUE or FALSE");
    if (isNull(l) || !length(l)) return(l);
    if (TYPEOF(l) != VECSXP) error("Input to rbindlist must be a list of data.tables");
    
    usenames = LOGICAL(sexp_usenames)[0];
    fill = LOGICAL(sexp_fill)[0];
    if (fill && !usenames) { 
        // override default
        warning("Resetting 'usenames' to TRUE. 'usenames' can not be FALSE when 'fill' is set to TRUE.\n");
        usenames=TRUE;
    }
    // check for factor, get max types, and when usenames=TRUE get the answer 'names' and column indices for proper reordering.
    preprocess(l, usenames, fill, &data);
    fnames   = VECTOR_ELT(data.ans_ptr, 0);
    findices = VECTOR_ELT(data.ans_ptr, 1);
    protecti = (usenames) ? 3 : 2; // to take care of PROTECTs in 'preprocess'
    if (data.n_rows == 0 && data.n_cols == 0) return(R_NilValue);

    factorLevels = PROTECT(allocVector(VECSXP, data.lcount));
    isRowOrdered = Calloc(data.lcount, Rboolean);
    
    ans = PROTECT(allocVector(VECSXP, data.n_cols)); protecti++;
    setAttrib(ans, R_NamesSymbol, fnames);
    lf = VECTOR_ELT(l, data.first);
    for(j=0; j<data.n_cols; j++) {
        if (fill) target = allocNAVector(data.max_type[j], data.n_rows);
        else target = allocVector(data.max_type[j], data.n_rows);
        SET_VECTOR_ELT(ans, j, target);
        
        if (usenames) {
            to_copy = TRUE;
            f_ind   = VECTOR_ELT(findices, j);
        } else {
            thiscol = VECTOR_ELT(lf, j);
            if (!isFactor(thiscol)) copyMostAttrib(thiscol, target); // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.
        }
        ansloc = 0;
        jj = 0; // to increment factorLevels
        resi = -1; 
        for (i=data.first; i<length(l); i++) {
            li = VECTOR_ELT(l,i);
            if (!length(li)) continue;  // majority of time though, each item of l is populated
            thislen = data.fn_rows[i];
            idx = (usenames) ? INTEGER(f_ind)[i] : j;
            if (idx < 0) {
                ansloc += thislen;
                resi++;
                if (data.is_factor[j]) {
                    isRowOrdered[resi] = FALSE;
                    SET_VECTOR_ELT(factorLevels, jj, allocNAVector(data.max_type[j], 1)); // the only level here is NA.
                    jj++;
                }
                continue;
            }
            thiscol = VECTOR_ELT(li, idx);
            // couldn't figure out a way to this outside this loop when fill = TRUE.
            if (to_copy && !isFactor(thiscol)) {
                copyMostAttrib(thiscol, target);
                to_copy = FALSE;
            }
            if (thislen != length(thiscol)) error("Column %d of item %d is length %d, inconsistent with first column of that item which is length %d. rbind/rbindlist doesn't recycle as it already expects each item to be a uniform list, data.frame or data.table", j+1, i+1, length(thiscol), thislen);
            resi++;  // after the first, there might be NULL or empty which are skipped, resi increments up until lcount
            if (TYPEOF(thiscol) != TYPEOF(target) && !isFactor(thiscol)) {
                thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));
                coerced = TRUE;
                // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
                // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
            }
            switch(TYPEOF(target)) {
            case STRSXP :
                isRowOrdered[resi] = FALSE;
                if (isFactor(thiscol)) {
                    levels = getAttrib(thiscol, R_LevelsSymbol);
                    for (r=0; r<thislen; r++)
                        if (INTEGER(thiscol)[r]==NA_INTEGER)
                            SET_STRING_ELT(target, ansloc+r, NA_STRING);
                        else
                            SET_STRING_ELT(target, ansloc+r, STRING_ELT(levels,INTEGER(thiscol)[r]-1));
    
                    // add levels to factorLevels
                    // changed "i" to "jj" and increment 'jj' after so as to fill only non-empty tables with levels
                    SET_VECTOR_ELT(factorLevels, jj, levels); jj++;
                    if (isOrdered(thiscol)) isRowOrdered[resi] = TRUE;
                } else {
                    if (TYPEOF(thiscol) != STRSXP) error("Internal logical error in rbindlist.c (not STRSXP), please report to datatable-help.");
                    for (r=0; r<thislen; r++) SET_STRING_ELT(target, ansloc+r, STRING_ELT(thiscol,r));
    
                    // if this column is going to be a factor, add column to factorLevels
                    // changed "i" to "jj" and increment 'jj' after so as to fill only non-empty tables with levels
                    if (data.is_factor[j]) {
                        SET_VECTOR_ELT(factorLevels, jj, thiscol);
                        jj++;
                    }
                    // removed 'coerced=FALSE; UNPROTECT(1)' as it resulted in a stack imbalance.
                    // anyways it's taken care of after the switch. So no need here.
                }
                break;
            case VECSXP :
                if (TYPEOF(thiscol) != VECSXP) error("Internal logical error in rbindlist.c (not VECSXP), please report to datatable-help.");
                for (r=0; r<thislen; r++)
                    SET_VECTOR_ELT(target, ansloc+r, VECTOR_ELT(thiscol,r));
                break;
            case REALSXP:
            case INTSXP:
            case LGLSXP:
                if (TYPEOF(thiscol) != TYPEOF(target)) error("Internal logical error in rbindlist.c, type of 'thiscol' should have already been coerced to 'target'. Please report to datatable-help.");
                memcpy((char *)DATAPTR(target) + ansloc * SIZEOF(thiscol),
                       (char *)DATAPTR(thiscol),
                       thislen * SIZEOF(thiscol));
                break;
            default :
                error("Unsupported column type '%s'", type2char(TYPEOF(target))); 
            }
            ansloc += thislen;
            if (coerced) {
                UNPROTECT(1);
                coerced = FALSE;
            }
        }
        if (data.is_factor[j]) {
            finalFactorLevels = combineFactorLevels(factorLevels, &(data.is_factor[j]), isRowOrdered);
            SEXP factorLangSxp = PROTECT(lang3(install(data.is_factor[j] == 1 ? "factor" : "ordered"),
                                               target, finalFactorLevels));
            SET_VECTOR_ELT(ans, j, eval(factorLangSxp, R_GlobalEnv));
            UNPROTECT(2);  // finalFactorLevels, factorLangSxp
        }
    }
    if (factorLevels != R_NilValue) UNPROTECT_PTR(factorLevels);

    Free(data.max_type);
    Free(data.is_factor);
    Free(data.fn_rows);
    Free(isRowOrdered);
    UNPROTECT(protecti);
    return(ans);
}

// A (relatively) fast (uses DT grouping) wrapper for matching two vectors, BUT:
// it behaves like 'pmatch' but only the 'exact' matching part. That is, a value in 
// 'x' is matched to 'table' only once. No index will be present more than once. 
// This should make it even clearer:
// chmatch2(c("a", "a"), c("a", "a")) # 1,2 - the second 'a' in 'x' has a 2nd match in 'table'
// chmatch2(c("a", "a"), c("a", "b")) # 1,NA - the second one doesn't 'see' the first 'a'
// chmatch2(c("a", "a"), c("a", "a.1")) # 1,NA - this is where it differs from pmatch - we don't need the partial match.
SEXP chmatch2(SEXP x, SEXP table, SEXP nomatch) {
    
    R_len_t i, j, k, nx, li, si, oi;
    SEXP dt, l, ans, order, start, lens, grpid, index;
    if (TYPEOF(x) != STRSXP) error("'x' must be a character vector");
    if (TYPEOF(table) != STRSXP) error("'table' must be a character vector");
    if (TYPEOF(nomatch) != INTSXP || length(nomatch) != 1) error("'nomatch' must be an integer of length 1");
    if (!length(x)) return(allocVector(INTSXP, 0));
    nx=length(x);
    if (!length(table)) {
        ans = PROTECT(allocVector(INTSXP, nx));
        for (i=0; i<nx; i++) INTEGER(ans)[i] = INTEGER(nomatch)[0];
        UNPROTECT(1);
        return(ans);
    }
    // Done with special cases. On to the real deal.
    l = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(l, 0, x);
    SET_VECTOR_ELT(l, 1, table);
    
    UNPROTECT(1); // l
    dt = PROTECT(unlist2(l));

    // order - first time
    order = PROTECT(fast_order(dt, 2));
    start = PROTECT(getAttrib(order, mkString("starts")));
    lens  = PROTECT(uniq_lengths(start, length(order))); // length(order) = nrow(dt)
    grpid = VECTOR_ELT(dt, 1); // dt[2] is unused here.
    index = VECTOR_ELT(dt, 2);
    
    // replace dt[1], we don't need it anymore
    k=0;
    for (i=0; i<length(lens); i++) {
        for (j=0; j<INTEGER(lens)[i]; j++) {
            INTEGER(grpid)[INTEGER(order)[k+j]-1] = j;
        }
        k += j;
    }
    // order - again
    UNPROTECT(3); // order, start, lens
    order = PROTECT(fast_order(dt, 2)); 
    start = PROTECT(getAttrib(order, mkString("starts")));
    lens  = PROTECT(uniq_lengths(start, length(order)));
    
    ans = PROTECT(allocVector(INTSXP, nx));
    k = 0;
    for (i=0; i<length(lens); i++) {
        li = INTEGER(lens)[i];
        si = INTEGER(start)[i]-1;
        oi = INTEGER(order)[si]-1;
        if (oi > nx-1) continue;
        INTEGER(ans)[oi] = (li == 2) ? INTEGER(index)[INTEGER(order)[si+1]-1]+1 : INTEGER(nomatch)[0];
    }
    UNPROTECT(5); // order, start, lens, ans
    return(ans);
}