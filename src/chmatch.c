#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

// chmatch() uses truelength on CHARSXP, being careful to store and restore R's own usage of truelength of CHARSXP
extern void savetl_init(), savetl(SEXP s), savetl_end();

#define ENC_KNOWN(x) (LEVELS(x) & 76)
// LATIN1_MASK (1<<2) | UTF8_MASK (1<<3) | ASCII_MASK (1<<6)

SEXP match_logical(SEXP table, SEXP x) {
    R_len_t i;
    SEXP ans, m;
    ans = PROTECT(allocVector(LGLSXP, length(x)));
    m = PROTECT(match(table, x, 0)); // nomatch=0
    for (i=0; i<length(x); i++)
        INTEGER(ans)[i] = INTEGER(m)[i] > 0;
    UNPROTECT(2);
    return(ans);
}

SEXP chmatch(SEXP x, SEXP table, R_len_t nomatch, Rboolean in) {
    R_len_t i, m;
    SEXP ans, s;
    if (!isString(x) && !isNull(x)) error("x is type '%s' (must be 'character' or NULL)", type2char(TYPEOF(x)));
    if (!isString(table) && !isNull(table)) error("table is type '%s' (must be 'character' or NULL)", type2char(TYPEOF(table)));
    PROTECT(ans = allocVector(in ? LGLSXP : INTSXP,length(x))); // if fails, it fails before savetl
    savetl_init();
    for (i=0; i<length(x); i++) {
        s = STRING_ELT(x,i);
        if (s != NA_STRING && ENC_KNOWN(s) != 64) { // PREV: s != NA_STRING && !ENC_KNOWN(s) - changed to fix for bug #5159. The previous fix 
                                           // dealt with UNKNOWN encodings. But we could have the same string, where both are in different 
                                           // encodings than ASCII (ex: UTF8 and Latin1). To fix this, we'll to resort to 'match' if not ASCII.
                                           // This takes care of all anomalies. It's unfortunate the 'chmatch' can't be used in these cases...
                                           // // fix for the 2nd part of bug #5159

            // explanation for PREV case: (still useful to keep it here)
            // symbols (i.e. as.name() or as.symbol()) containing non-ascii characters (>127) seem to be 'unknown' encoding in R.
            // The same string in different encodings (where unknown encoding is different to known, too) are different
            // CHARSXP pointers; this chmatch relies on pointer equality only. We tried mkChar(CHAR(s)) [ what install() does ]
            // but this impacted the fastest cases too much. Hence fall back to match() on the first unknown encoding detected
            // since match() considers the same string in different encodings as equal (but slower). See #2538 and #4818. 
            savetl_end();
            UNPROTECT(1);
            return (in ? match_logical(table, x) : match(table, x, nomatch));
        }
        if (TRUELENGTH(s)>0) savetl(s);
        // as from v1.8.0 we assume R's internal hash is positive. So in R < 2.14.0 we
        // don't save the uninitialised truelengths that by chance are negative, but
        // will save if positive. Hence R >= 2.14.0 may be faster and preferred now that R
        // initializes truelength to 0 from R 2.14.0.
        SET_TRUELENGTH(s,0);
    }
    for (i=length(table)-1; i>=0; i--) {
        s = STRING_ELT(table,i);
        if (s != NA_STRING && ENC_KNOWN(s) != 64) { // changed !ENC_KNOWN(s) to !ASCII(s) - check above for explanation	
            for (int j=i+1; j<LENGTH(table); j++) SET_TRUELENGTH(STRING_ELT(table,j),0);  // reinstate 0 rather than leave the -i-1
            savetl_end();
            UNPROTECT(1);
            return (in ? match_logical(table, x) : match(table, x, nomatch));
        }
        if (TRUELENGTH(s)>0) savetl(s);
        SET_TRUELENGTH(s, -i-1);
    }
    if (in) {
        for (i=0; i<length(x); i++) {
            LOGICAL(ans)[i] = TRUELENGTH(STRING_ELT(x,i))<0;
            // nomatch ignored for logical as base does I think
        }
    } else {
        for (i=0; i<length(x); i++) {
            m = TRUELENGTH(STRING_ELT(x,i));
            INTEGER(ans)[i] = (m<0) ? -m : nomatch;
        }
    }
    for (i=0; i<length(table); i++)
        SET_TRUELENGTH(STRING_ELT(table,i),0);  // reinstate 0 rather than leave the -i-1
    savetl_end();
    UNPROTECT(1);
    return(ans);
}

SEXP chmatchwrapper(SEXP x, SEXP table, SEXP nomatch, SEXP in) {
    return(chmatch(x,table,INTEGER(nomatch)[0],LOGICAL(in)[0]));
}

