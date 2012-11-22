#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <fcntl.h>
#include <stdio.h>

// TO DO: secondary separator for list() columns, such as columns 11 and 12 in BED.
// TO DO: loop fscanf in batches of 32 (or 128) to remove column limit
// TO DO: test if increasing p from 32 to 128 decreases performance for 10 columns.
// TO DO: whitespace at the end of the line before \n, should be ignored
// TO DO: invalid but not ,, data should be reported as warning (and NA as already done).
// TO DO: should read int64 / bit64 without going via character
// TO DO: test using "grep read.table ...Rtrunk/tests/*"

extern int sizes[];
SEXP readfile(SEXP fnam, SEXP formatarg, SEXP types, SEXP skip, SEXP estnarg, SEXP verbosearg)
{
    SEXP thiscol, ans;
    R_len_t i, protecti=0, nrow=0, ncol, estn;
    int size[32], type[32], thistype, nc=0, charcol[32], c1, c2, c3;
    char *buffer[32];
    char *p[32], *format, *formatcopy, *fmttok[32], *q;
    long pos=0;
    Rboolean verbose = LOGICAL(verbosearg)[0];
    if (LENGTH(types)>32) error("Currently limited to 32 columns just for dev. Will be increased.");
    for (i=0; i<32; i++) p[i] = 0;
    FILE *f=fopen(CHAR(STRING_ELT(fnam,0)),"r");
    if (f == NULL) error("file not found");
    estn = INTEGER(estnarg)[0];
    ncol = LENGTH(types);
    ans=PROTECT(allocVector(VECSXP,ncol));  // TO DO, could over-allocate here directly, or maybe safer to go via alloccol.
    protecti++;
    for (i=0; i<ncol; i++) {
        thistype = type[i] = TYPEOF(VECTOR_ELT(types,i));
        thiscol = PROTECT(allocVector(thistype,estn));
        protecti++;
        SET_TRUELENGTH(thiscol, estn);
        SET_VECTOR_ELT(ans,i,thiscol);
        if (thistype == STRSXP) {
            charcol[nc] = i;
            p[i] = buffer[nc] = malloc(sizeof(char)*1024);   // TO DO tie in %1024s and return value from fscanf
            size[i] = 0;
            nc++;
        } else {
            p[i] = (char *)DATAPTR(thiscol);
            size[i] = sizes[thistype];
        }
    }
    for (i=0; i<INTEGER(skip)[0]; i++) while (fgetc(f) != '\n');
    format = (char *)CHAR(STRING_ELT(formatarg,0));
    formatcopy = malloc(sizeof(char)*strlen(format));
    strcpy(formatcopy,format);
    fmttok[i=0] = q = strtok(formatcopy,",");
    while (q != NULL) {
        if (q[1] == '[') {
            q[strlen(q)] = ',';
            strtok(NULL,",");
        }
        fmttok[++i] = q = strtok(NULL,",");
    }
    if (verbose) {Rprintf("Split format (if last col is character, extra newline will print here):"); for (i=0; i<ncol; i++){Rprintf(" %s", fmttok[i]);}}
    while ((i=fscanf(f, format,
            p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7],            // see footnote 1
            p[8], p[9], p[10],p[11],p[12],p[13],p[14],p[15],
            p[16],p[17],p[18],p[19],p[20],p[21],p[22],p[23],
            p[24],p[25],p[26],p[27],p[28],p[29],p[30],p[31])) != EOF) {
        while (i<ncol) {
            switch(type[i]) {              // this switch could be avoided for speed using memcpy, but less readable
            case INTSXP:                   // and this only runs for each invalid or missing field anyway (i.e. rarely)
               *(int *)p[i] = NA_INTEGER;
               break;
            case REALSXP:
               *(double *)p[i] = NA_REAL;
               if (ftell(f)>pos) {                          // see footnote 2. dealing with potential scanf bug, not needed otherwise
                   c3 = getc(f); fseek(f,-3,SEEK_CUR); c1 = getc(f); c2 = getc(f);   
                   if ((c1=='A' || c1=='a') && c2==',' &&   // has just potentially over-read comma of "NA,"
                       (pos ||                              // pos>0 => not the first NA in this row. ftell below has just run and
                                                            //   the one-by-one fscanf did just advance (i.e. didn't just read ",,")
                        (c3!=',' || type[i-1]!=STRSXP)))    // if the all-on-one fscanf above just read NA into a character column
                                                            //   but a float ",," is next, the comma wasn't over read so don't rewind
                       fseek(f,-1,SEEK_CUR);                // rewind the over read comma when NA appears in a float column
               }
               break;
            case STRSXP:
               *p[i] = '\0';
               break;
            default:
               error("Unsupported type");
            }
            while(getc(f)!=',');   // discard remainder of invalid input in this field (if any) and read next separator
            while (++i<ncol && (pos=ftell(f)) && fscanf(f, fmttok[i], p[i]) && getc(f)!=EOF);   // read remaining fields one by one
            // TO DO - cater for EOF here on last line of file if there's a missing there,  and 2 missings in the same row.
        }
        for (i=0; i<ncol; i++) p[i]+=size[i];
        for (i=0; i<nc; i++) SET_STRING_ELT(VECTOR_ELT(ans, charcol[i]), nrow, mkChar(buffer[i]));
        pos=0;
        nrow++;
    }
    fclose(f);
    for (i=0; i<ncol; i++) SETLENGTH(VECTOR_ELT(ans,i), nrow);
    UNPROTECT(protecti);
    for (i=0; i<nc; i++) free(buffer[i]);
    free(formatcopy);
    return(ans);
}

/*
Footnote 1.
Often superfluous and ugly p[*] repetition is deliberate and required.
It isn't possible in C to construct a vargs and pass it to fscanf in C or C99. A '...' can
be passed on, but not created, unless we depend on a specialist library or go to asm.
Assumption is that fscanf is optimised internally for navigating the format, and is faster than character by
character code with branching and function calls at the C level (such as in R's scan.c).

Footnote 2.
The internet has many (usually false) claims of bug finds in fscanf, but this _might_ actually
be one. NAN and INF (case insensitive) are (the only) acceptable text values under IEEE Binary Floating Point
format (vs hexadecimal floating-point) to %g (and %e %f %a equivalents). That capabability is
deep in the OS or hardware (exactly which can vary) and we like taking advantage of that, by using fscanf.
However, fscanf is supposed to stop reading the field on invalid character
and leave that invalid character unread. It does this for all other invalid character input to %g. But
when it reads "NA", apparently, it expects N next (to form NAN) realises "," isn't valid and stops ok but doesn't leave that comma
unread (inconsistent with all other invalid input to %g).  Hence the rewind and special case in the C code above.
Unfortunately, R historically defaulted to writing out "NA" for NA, so we have to cope with
that. Just blank (i.e. ,,) would have been easier. It's not so bad as the extra test only runs when
a missing or invalid input to %lg actually occurs; i.e., for fully populated files not written by R, that'll never run.
Note the test on getc only needs to test for "A," or "a,", since the only way that could happen if there was an N (or n) before that; i.e., it really is only a special case for this apparent bug in scanf with NA (not NAN) input.
The ftell(f)>pos is to deal with ",NA,," to detect that the 2nd field hasn't advanced (without that if, it would correct the comma overread bug in scanf a 2nd time for the same particular comma, and a mess would ensue).

TO DO: link to gcc/S.O. query.
*/


