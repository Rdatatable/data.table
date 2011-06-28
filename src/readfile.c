#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rdefines.h>
#include <fcntl.h>
#include <stdio.h>

#ifdef BUILD_DLL
// For Windows only
#define EXPORT __declspec(dllexport)
EXPORT SEXP readfile();
#endif

SEXP readfile(SEXP fnam)
{
    SEXP c1,c2,ans;
    void *p[64];
    int i, res, nrow=0;
    FILE *f=fopen(CHAR(STRING_ELT(fnam,0)),"r");
    if (f == NULL) error("file not found");
    ans=PROTECT(allocVector(VECSXP,2));  // 2 column data.table, first steps
    c1=PROTECT(allocVector(INTSXP,1000000));  // hard coded to match n=1e6 test up in read.R
    c2=PROTECT(allocVector(INTSXP,1000000));
    SET_VECTOR_ELT(ans,0,c1);
    SET_VECTOR_ELT(ans,1,c2);
    for (i=0;i<64;i++) p[i] = 0;
    p[0] = DATAPTR(c1);
    p[1] = DATAPTR(c2);
    char format[100]="%d,%d\n";
    while (fscanf(f,format, p[0], p[1], p[2], p[3])!=EOF) {
        // Unused and zero'd p[2], p[3] (, ..., p[64]
        // to be added) is deliberate.
        // It isn't possible in C to construct a vargs
        // and pass it to fscanf in C or C99. A '...' can
        // be passed on, but not created unless we depend
        // on a specialist library or go to asm.
        // Living with a, say, 64 column limit, seems fine for many
        // cases. Or, perhaps fscanf could be called
        // twice per row to read in file with 128 columns, etc.
        // Assumption is that fscanf is optimised internally for
        // navigating the format, and is faster than character by
        // character code with branching and function calls at the
        // C level (such as in scan.c).
        // Already with only 2 columns we see a 4 times speedup
        // and perhaps with more columns this factor will be greater.
        nrow++;
        for (i=0;i<2;i++) p[i]+=4;
        // TO DO: secondary separator for list() columns, such as
        // columns 11 and 12 in BED.
    }
    fclose(f);
    LENGTH(c1) = nrow;
    LENGTH(c2) = nrow;
    UNPROTECT(3);
    return(ans);
}


