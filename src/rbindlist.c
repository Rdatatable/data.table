#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>

int sizes[100];
#define SIZEOF(x) sizes[TYPEOF(x)]

int StrCmp(SEXP x, SEXP y);    // in countingcharacter.c

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
    R_xlen_t nmax;
    hlen (*hash)(SEXP, R_xlen_t, HashData *);
    int (*equal)(SEXP, R_xlen_t, SEXP, R_xlen_t);
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
static hlen cshash(SEXP x, R_xlen_t indx, HashData *d)
{
    intptr_t z = (intptr_t) STRING_ELT(x, indx);
    unsigned int z1 = (unsigned int)(z & 0xffffffff), z2 = 0;
#if SIZEOF_LONG == 8
    z2 = (unsigned int)(z/0x100000000L);
#endif
    return scatter(z1 ^ z2, d);
}

static hlen shash(SEXP x, R_xlen_t indx, HashData *d)
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

static int sequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j)
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
static void MKsetup(HashData *d, R_xlen_t n)
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
static void HashTableSetup(HashData *d, R_xlen_t n)
{
    d->hash = shash;
    d->equal = sequal;
    MKsetup(d, n);
    d->HashTable = malloc(sizeof(struct llist *) * (d->M));
    for (R_xlen_t i = 0; i < d->M; i++) d->HashTable[i] = NULL;
}

static void CleanHashTable(HashData *d)
{
    struct llist * root, * tmp;

    for (R_xlen_t i = 0; i < d->M; ++i) {
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
SEXP combineFactorLevels(SEXP factorLevels, int * factorType, Rboolean * isRowOrdered) {
    // find total length
    R_xlen_t size = 0;
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
                            if (locs[pl->i] >= LENGTH(tmp)) {
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
                        } while (pl = pl->next);
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
                        } while (pl = pl->next);
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


SEXP rbindlist(SEXP l)
{
    R_len_t i,j,r, nrow=0, first=-1, ansloc, ncol=0, thislen;
    SEXP ans, li, lf=R_NilValue, thiscol, target, levels;
    int size;
    Rboolean coerced=FALSE;
    SEXPTYPE * maxtype = NULL;     // TODO: should memory allocation be done differently from malloc/free? (also in a few places above and below)
    int * isColFactor = NULL;      // for each column this is 0 if not a factor, 1 if a factor, and 2 if an ordered factor
    Rboolean * isRowOrdered = NULL;
    SEXPTYPE type;
    SEXP factorLevels = R_NilValue, finalFactorLevels;
    
    for (i=0;i<length(l);i++) {
        li = VECTOR_ELT(l,i);
        if (isNull(li)) continue;
        if (TYPEOF(li) != VECSXP) error("Item %d of list input is not a data.frame, data.table or list",i+1);
        if (!LENGTH(li) || !length(VECTOR_ELT(li,0))) continue;
        if (first==-1) {
            first = i;   // First non-empty list/data.frame/data.table
            lf = li;
            ncol = length(lf);

            // initialize factorLevels - will keep factor levels for factors and the entire column for everything else
            factorLevels = PROTECT(allocVector(VECSXP, LENGTH(l)));

            // initialize the max types - will possibly increment later
            maxtype = malloc(ncol * sizeof(SEXPTYPE));
            isColFactor = malloc(ncol * sizeof(int));
            isRowOrdered = malloc(LENGTH(l) * sizeof(Rboolean));
            for (j = 0; j < ncol; ++j) {
                if (isFactor(thiscol)) {
                    if (isOrdered(thiscol)) {
                        isColFactor[j] = 2;
                    } else {
                        isColFactor[j] = 1;
                    }
                    maxtype[j] = STRSXP;   // if any column is a factor everything will be converted to strings and factorized at the end
                } else {
                    isColFactor[j] = 0;
                    maxtype[j] = TYPEOF(VECTOR_ELT(lf, j));
                }
            }
        } else {
            if (length(li) != ncol) error("Item %d has %d columns, inconsistent with item %d which has %d columns",i+1,length(li),first+1,ncol);
        }
        nrow+=LENGTH(VECTOR_ELT(li,0));

        for (j = 0; j < ncol; ++j) {
            if (isColFactor[j] == 2) continue;

            thiscol = VECTOR_ELT(li, j);
            if (isFactor(thiscol)) {
                if (isOrdered(thiscol)) {
                    isColFactor[j] = 2;
                } else {
                    isColFactor[j] = 1;
                }
                maxtype[j] = STRSXP;   // if any column is a factor everything will be converted to strings and factorized at the end
            } else {
                type = TYPEOF(thiscol);
                if (type > maxtype[j]) maxtype[j] = type;
            }
        }
    }
    PROTECT(ans = allocVector(VECSXP, ncol));
    setAttrib(ans, R_NamesSymbol, getAttrib(lf, R_NamesSymbol));
    for(j=0; j<ncol; j++) {
        thiscol = VECTOR_ELT(lf,j);
        target = allocVector(maxtype[j], nrow);
        if (!isFactor(thiscol))
          copyMostAttrib(thiscol, target);  // all but names,dim and dimnames. And if so, we want a copy here, not keepattr's SET_ATTRIB.

        SET_VECTOR_ELT(ans, j, target);
        ansloc = 0;
        for (i=first; i<length(l); i++) {
            li = VECTOR_ELT(l,i);
            if (!length(li)) continue;  // majority of time though, each item of l is populated
            thislen = length(VECTOR_ELT(li,0));  // type of li was checked to be VECSXP already above
            if (!thislen) continue;
            thiscol = VECTOR_ELT(li,j);
            if (thislen != length(thiscol)) error("Column %d of item %d is length %d, inconsistent with first column of that item which is length %d. rbindlist doesn't recycle as it already expects each item to be a uniform list, data.frame or data.table", j+1, i+1, length(thiscol), thislen);
            
            if (TYPEOF(thiscol) != TYPEOF(target) && !isFactor(thiscol)) {
                thiscol = PROTECT(coerceVector(thiscol, TYPEOF(target)));
                coerced = TRUE;
                // TO DO: options(datatable.pedantic=TRUE) to issue this warning :
                // warning("Column %d of item %d is type '%s', inconsistent with column %d of item %d's type ('%s')",j+1,i+1,type2char(TYPEOF(thiscol)),j+1,first+1,type2char(TYPEOF(target)));
            }
            switch(TYPEOF(target)) {
            case STRSXP :
                isRowOrdered[i] = FALSE;
                if (isFactor(thiscol)) {
                    levels = getAttrib(thiscol, R_LevelsSymbol);
                    for (r=0; r<thislen; r++)
                        if (INTEGER(thiscol)[r]==NA_INTEGER)
                            SET_STRING_ELT(target, ansloc+r, NA_STRING);
                        else
                            SET_STRING_ELT(target, ansloc+r, STRING_ELT(levels,INTEGER(thiscol)[r]-1));

                    // add levels to factorLevels
                    SET_VECTOR_ELT(factorLevels, i, levels);
                    if (isOrdered(thiscol)) isRowOrdered[i] = TRUE;
                } else {
                    if (TYPEOF(thiscol) != STRSXP) error("Internal logical error in rbindlist.c (not STRSXP), please report to datatable-help.");
                    for (r=0; r<thislen; r++) SET_STRING_ELT(target, ansloc+r, STRING_ELT(thiscol,r));

                    // if this column is going to be a factor, add column to factorLevels
                    if (isColFactor[j]) {
                        SET_VECTOR_ELT(factorLevels, i, thiscol);
                    }
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
                if (TYPEOF(thiscol) != TYPEOF(target)) error("Internal logical error in rbindlist.c (thiscol's type should have been coerced to target), please report to datatable-help.");
                size = SIZEOF(thiscol);
                memcpy((char *)DATAPTR(target) + ansloc*size,
                       (char *)DATAPTR(thiscol),
                       thislen * size);
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
        if (isColFactor[j]) {
            finalFactorLevels = combineFactorLevels(factorLevels, &isColFactor[j], isRowOrdered);

            SEXP factorLangSxp = PROTECT(lang3(install(isColFactor[j] == 1 ? "factor" : "ordered"),
                                               target, finalFactorLevels));
            SET_VECTOR_ELT(ans, j, eval(factorLangSxp, R_GlobalEnv));
            UNPROTECT(2);  // finalFactorLevels, factorLangSxp
        }
    }
    UNPROTECT(2);  // ans, factorLevels

    free(maxtype);
    free(isColFactor);
    free(isRowOrdered);

    return(ans);
}


