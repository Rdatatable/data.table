#include "data.table.h"
#include <Rdefines.h>
#include <time.h>
// #include <signal.h> // the debugging machinery + breakpoint aidee

// TODO: rewrite/simplify logic -- took me ages to understand what I wrote!!
// TODO: benchmark and parallelise slow regions
// TODO: implement 'lookup' for 'gaps' and 'overlaps' arguments
SEXP lookup(SEXP ux, SEXP xlen, SEXP indices, SEXP gaps, SEXP overlaps, SEXP multArg, SEXP typeArg, SEXP verbose) {

  SEXP vv, tt, lookup, type_lookup;
  R_len_t i,j,k,*idx,*count,*type_count,xrows=INTEGER(xlen)[0],uxrows=LENGTH(VECTOR_ELT(ux, 0)),uxcols=LENGTH(ux);
  int *from = (int *)INTEGER(VECTOR_ELT(indices, 0));
  int *to   = (int *)INTEGER(VECTOR_ELT(indices, 1));
  clock_t pass1, pass2, pass3, start;
  enum {ALL, FIRST, LAST} mult = ALL;
  enum {ANY, WITHIN, START, END, EQUAL} type = ANY;

  if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "all"))  mult = ALL;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "first")) mult = FIRST;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "last")) mult = LAST;
  else error(_("Internal error: invalid value for 'mult'; this should have been caught before. please report to data.table issue tracker")); // # nocov

  if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "any"))  type = ANY;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "within")) type = WITHIN;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "start")) type = START;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "end")) type = END;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "equal")) type = EQUAL;
  else error(_("Internal error: invalid value for 'type'; this should have been caught before. please report to data.table issue tracker")); // # nocov

  // For reference: uxcols-1 = type_count, uxcols-2 = count, uxcols-3 = type_lookup, uxcols-4 = lookup
  // first pass: calculate lengths first
  start = clock();
  count = (int *)INTEGER(VECTOR_ELT(ux, uxcols-2));
  type_count = (int *)INTEGER(VECTOR_ELT(ux, uxcols-1));
  switch (mult) {
  case FIRST:
    switch(type) {
    case EQUAL:
      for (i=0; i<xrows; i++) {
        count[from[i]-1]++; count[to[i]-1]++;
        type_count[from[i]-1]++; type_count[to[i]-1]++;
      }
      break;
    case START: case END: case ANY: case WITHIN:
      for (i=0; i<xrows; i++) {
        for (j=from[i]; j<=to[i]; j++) {
          count[j-1]++;
        }
      }
      if (type != WITHIN) {
        for (i=0; i<uxrows; i++)                      // TODO: this allocation can be avoided if we take care of FIRST/LAST accordingly in 'overlaps'
          if (count[i]) type_count[i] = 1;
      }
      break;
    default: error(_("Internal error: unknown type in mult=%d in lookup: %d"), mult, type); // #nocov
    }
    break;

  case LAST :
    switch (type) {
    case ANY:
      for (i=0; i<xrows; i++) {
        for (j=from[i]; j<=to[i]; j++) {
          count[j-1]++;
          if (from[i]==j && !type_count[j-1]) type_count[j-1]++;
        }
      }
      break;
    case EQUAL:
      for (i=0; i<xrows; i++) {
        count[from[i]-1]++; count[to[i]-1]++;
        type_count[from[i]-1]++; type_count[to[i]-1]++;
      }
      break;
    case START: case END: case WITHIN:
      for (i=0; i<xrows; i++) {
        for (j=from[i]; j<=to[i]; j++) {
          count[j-1]++;
        }
      }
      if (type != WITHIN) {
        for (i=0; i<uxrows; i++)              // TODO: this allocation can be avoided if we take care of FIRST/LAST accordingly in 'overlaps'
          if (count[i]) type_count[i] = 1;
      }
      break;
    }
    break;

  case ALL :
    switch (type) {
    case START: case END:
      for (i=0; i<xrows; i++) {
        for (j=from[i]; j<=to[i]; j++) {
          count[j-1]++; type_count[j-1]++;       // alternatively, we could simply do with type_count=count ?
        }
      }
      break;
    case EQUAL:
      for (i=0; i<xrows; i++) {
        count[from[i]-1]++; count[to[i]-1]++;
        type_count[from[i]-1]++; type_count[to[i]-1]++;
      }
      break;
    case ANY :
      for (i=0; i<xrows; i++) {
        k = from[i];
        for (j=from[i]; j<=to[i]; j++) {
          count[j-1]++;
          if (k==j) type_count[j-1]++;
        }
      }
      break;
    case WITHIN :
      for (i=0; i<xrows; i++) {
        for (j=from[i]; j<=to[i]; j++) {
          count[j-1]++;
        }
      }
      break;
    default: error(_("Internal error: unknown type in mult=%d in lookup: %d"), mult, type); // #nocov
    }
    break;
  default: error(_("Internal error: unknown mult in lookup: %d"), mult); // #nocov
  }
  pass1 = clock() - start;
  if (LOGICAL(verbose)[0])
    Rprintf(_("First pass on calculating lengths in lookup ... done in %8.3f seconds\n"), 1.0*(pass1)/CLOCKS_PER_SEC);
  // second pass: allocate vectors
  start = clock();
  lookup = VECTOR_ELT(ux, uxcols-4);
  type_lookup = VECTOR_ELT(ux, uxcols-3);
  for (i=0; i<uxrows; i++) {
    SET_VECTOR_ELT(lookup, i, vv=allocVector(INTSXP, count[i]));
    if (type != WITHIN) {
      SET_VECTOR_ELT(type_lookup, i, vv=allocVector(INTSXP, type_count[i]));
    }
  }
  pass2 = clock() - start;
  if (LOGICAL(verbose)[0])
    Rprintf(_("Second pass on allocation in lookup ... done in %8.3f seconds\n"), 1.0*(pass2)/CLOCKS_PER_SEC);
  // generate lookup
  start = clock();
  idx = Calloc(uxrows, R_len_t); // resets bits, =0
  switch (type) {
  case ANY: case START: case END: case WITHIN:
    for (i=0; i<xrows; i++) {
      for (j=from[i]; j<=to[i]; j++) {
        vv = VECTOR_ELT(lookup, j-1);  // cache misses - memory efficiency? but 'lookups' are tiny - takes 0.036s on A.thaliana GFF for entire process)
        INTEGER(vv)[idx[j-1]++] = i+1;
      }
    }
    break;
  case EQUAL:
    for (i=0; i<xrows; i++) {
      INTEGER(VECTOR_ELT(lookup, from[i]-1))[idx[from[i]-1]++] = i+1;
      INTEGER(VECTOR_ELT(lookup, to[i]-1))[idx[to[i]-1]++] = i+1;
    }
    break;
  default: error(_("Internal error: unknown type lookup should have been caught earlier: %d"), type); // #nocov
  }
  Free(idx);
  // generate type_lookup
  if (type != WITHIN) {
    switch (mult) {
    case FIRST :
      for (i=0; i<uxrows; i++) {
        if (!count[i]) continue;
        vv = VECTOR_ELT(lookup, i);
        tt = VECTOR_ELT(type_lookup, i);
        if (length(tt) && length(vv)) {            // length check added by Matt to avoid SEGV in #2767
          INTEGER(tt)[0] = INTEGER(vv)[0];
        }
      }
      break;

    case LAST :
      for (i=0; i<uxrows; i++) {
        if (!count[i]) continue;
        vv = VECTOR_ELT(lookup, i);
        tt = VECTOR_ELT(type_lookup, i);
        if (length(tt) && length(vv)>=count[i]) {   // length check added by Matt to avoid SEGV in #2767
          INTEGER(tt)[0] = INTEGER(vv)[count[i]-1];
        }
      }

    case ALL :
      switch (type) {
      case START: case END: case EQUAL:
        for (i=0; i<uxrows; i++)
          SET_VECTOR_ELT(type_lookup, i, VECTOR_ELT(lookup, i));
        break;

      case ANY :
        for (i=0; i<uxrows; i++) {
          vv = VECTOR_ELT(lookup, i);
          tt = VECTOR_ELT(type_lookup, i);
          k=0;
          for (j=count[i]-type_count[i]; j<count[i]; j++)
            INTEGER(tt)[k++] = INTEGER(vv)[j];
        }
        break;

      case WITHIN :
        // for (i=0; i<uxrows; i++) {
        //     vv = VECTOR_ELT(lookup, i);
        //     tt = VECTOR_ELT(type_lookup, i);
        //     for (j=0; j<type_count[i]; j++)
        //         INTEGER(tt)[j] = INTEGER(vv)[j];
        // }
        break; // #nocov
      default: error(_("Internal error: unknown type in mult=%d in lookup should have been caught earlier: %d"), mult, type); // #nocov
      }
     break;
    default: error(_("Internal error: unknown mult in lookup: %d"), mult); // #nocov
    }
  }
  pass3 = clock() - start;
  if (LOGICAL(verbose)[0])
    Rprintf(_("Final step in generating lookup ... done in %8.3f seconds\n"), 1.0*(pass3)/CLOCKS_PER_SEC);
  return(R_NilValue);
}

SEXP overlaps(SEXP ux, SEXP imatches, SEXP multArg, SEXP typeArg, SEXP nomatchArg, SEXP verbose) {

  R_len_t i,j,k,m,uxcols=LENGTH(ux),rows=length(VECTOR_ELT(imatches,0));
  int nomatch = INTEGER(nomatchArg)[0], totlen=0, len, thislen, wlen=0;
  int *from   = (int *)INTEGER(VECTOR_ELT(imatches, 0));
  int *to     = (int *)INTEGER(VECTOR_ELT(imatches, 1));
  int *count   = (int *)INTEGER(VECTOR_ELT(ux, uxcols-2));
  int *type_count   = (int *)INTEGER(VECTOR_ELT(ux, uxcols-1));
  SEXP lookup = VECTOR_ELT(ux, uxcols-4);
  SEXP type_lookup = VECTOR_ELT(ux, uxcols-3);
  SEXP ans, f1__, f2__, tmp1, tmp2;
  clock_t end1, end2, start;
  enum {ALL, FIRST, LAST} mult = ALL;
  enum {ANY, WITHIN, START, END, EQUAL} type = ANY;
  // raise(SIGINT);

  if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "all"))  mult = ALL;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "first")) mult = FIRST;
  else if (!strcmp(CHAR(STRING_ELT(multArg, 0)), "last")) mult = LAST;
  else error(_("Internal error: invalid value for 'mult'; this should have been caught before. please report to data.table issue tracker")); // # nocov

  if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "any"))  type = ANY;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "within")) type = WITHIN;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "start")) type = START;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "end")) type = END;
  else if (!strcmp(CHAR(STRING_ELT(typeArg, 0)), "equal")) type = EQUAL;
  else error(_("Internal error: invalid value for 'type'; this should have been caught before. please report to data.table issue tracker")); // # nocov

  // As a first pass get the final length, so that we can allocate up-front and not deal with Calloc + Realloc + size calculation hassle
  // Checked the time for this loop on realisitc data (81m reads) and took 0.27 seconds! No excuses ;).
  start = clock();
  if (mult == ALL) {
    totlen=0;
    switch (type) {
    case START: case END:
      for (i=0; i<rows; i++)
        totlen += (from[i] > 0 && type_count[from[i]-1]) ? type_count[from[i]-1] : 1;
      break;

    case EQUAL:
      for (i=0; i<rows; i++) {
        len = totlen; wlen=0, j=0, m=0;
        k = (from[i]>0) ? from[i] : 1;
        if (k == to[i]) {
          wlen = count[k-1];
        } else if (k < to[i]) {
          tmp1 = VECTOR_ELT(lookup, k-1);
          tmp2 = VECTOR_ELT(type_lookup, to[i]-1);
          while (j<count[k-1] && m<type_count[to[i]-1]) {
            if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
              ++wlen; ++j; ++m;
            } else if ( INTEGER(tmp1)[j] > INTEGER(tmp2)[m] ) {
              ++m;
            } else ++j;
          }
        }
        totlen += wlen;
        if (len == totlen)
          ++totlen;
      }
      break;

    case ANY:
      for (i=0; i<rows; i++) {
        len = totlen;
        // k = (from[i] > 0) ? from[i] : 1;
        k = from[i];
        if (k<=to[i])
          totlen += count[k-1];
        for (j=k+1; j<=to[i]; j++)
          totlen += type_count[j-1];
        if (len == totlen)
          ++totlen;
      }
      break;

    case WITHIN:
      for (i=0; i<rows; i++) {
        len = totlen; j=0; m=0;
        k = from[i];
        if (k > 0) {
          if (k == to[i]) {
            totlen += count[k-1];
          } else if (k < to[i]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(lookup, to[i]-1);
            while (j<count[k-1] && m<count[to[i]-1]) {
              if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
                ++totlen; ++j; ++m;
              } else if ( INTEGER(tmp1)[j] > INTEGER(tmp2)[m] ) {
                ++m;
              } else ++j;
            }
          }
        }
        if (len == totlen)
          ++totlen;
      }
      break;
    default: error(_("Internal error: unknown type in mult=ALL in overlaps: %d"), mult, type); // #nocov
    }
  } else totlen = rows;
  end1 = clock() - start;
  if (LOGICAL(verbose)[0])
    Rprintf(_("First pass on calculating lengths in overlaps ... done in %8.3f seconds\n"), 1.0*(end1)/CLOCKS_PER_SEC);

  // ans[0] is the the position of 'query' and ans[1] is that of 'subject'
  // allocate f1__ and f2__ and assign 'nomatch' to f2__
  ans = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(ans, 0, f1__=allocVector(INTSXP, totlen));
  SET_VECTOR_ELT(ans, 1, f2__=allocVector(INTSXP, totlen));
  thislen=0;
  start = clock();

  // switching mult=ALL,FIRST,LAST separately to
  //   - enhance performance for special cases, and
  //   - easy to fix any bugs in the future
  switch (mult) {
  case ALL:
    switch (type) {
    case START : case END :
      for (i=0; i<rows; i++) {
        len = thislen;
        if (from[i] > 0) {
          k = from[i];
          tmp2 = VECTOR_ELT(type_lookup, k-1);
          for (j=0; j<type_count[k-1]; j++) {
            INTEGER(f1__)[thislen] = i+1;
            INTEGER(f2__)[thislen] = INTEGER(tmp2)[j];
            ++thislen;
          }
        }
        if (len == thislen) {
          INTEGER(f1__)[thislen] = i+1;
          INTEGER(f2__)[thislen] = nomatch;
          ++thislen;
        }
      }
      break;

    case EQUAL :
      for (i=0; i<rows; i++) {
        len = thislen;
        if (from[i] > 0 && to[i] > 0) {
          k = from[i];
          if (k == to[i]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(type_lookup, to[i]-1);
            for (j=0; j<count[k-1]; j++) {
              INTEGER(f1__)[thislen] = i+1;
              INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
              ++thislen;
            }
          } else if (k < to[i]) {
            j=0; m=0;
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(type_lookup, to[i]-1);
            while (j<count[k-1] && m<type_count[to[i]-1]) {
              if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
                INTEGER(f1__)[thislen] = i+1;
                INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
                 ++thislen; ++j; ++m;
               } else if ( INTEGER(tmp1)[j] > INTEGER(tmp2)[m] ) {
                 ++m;
               } else ++j;
             }
           }
         }
         if (len == thislen) {
           INTEGER(f1__)[thislen] = i+1;
           INTEGER(f2__)[thislen] = nomatch;
           ++thislen;
         }
      }
      break;

    case ANY :
      for (i=0; i<rows; i++) {
        len = thislen;
        // k = (from[i]>0) ? from[i] : 1;
        k = from[i];
        if (k<=to[i]) {
          tmp1 = VECTOR_ELT(lookup, k-1);
          for (m=0; m<count[k-1]; m++) {
            INTEGER(f1__)[thislen] = i+1;
            INTEGER(f2__)[thislen] = INTEGER(tmp1)[m];
            ++thislen;
          }
        }
        for (j=k+1; j<=to[i]; j++) {
          tmp2 = VECTOR_ELT(type_lookup, j-1);
          for (m=0; m<type_count[j-1]; m++) {
            INTEGER(f1__)[thislen] = i+1;
            INTEGER(f2__)[thislen] = INTEGER(tmp2)[m];
            ++thislen;
          }
        }
        // dint go through any loops above
        if (len == thislen) {
          INTEGER(f1__)[thislen] = i+1;
          INTEGER(f2__)[thislen] = nomatch;
          ++thislen;
        }
      }
      break;

    case WITHIN :
      for (i=0; i<rows; i++) {
        len = thislen;
        k=from[i];
        if (k > 0) {
          if (k == to[i]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            for (j=0; j<count[k-1]; j++) {
              INTEGER(f1__)[thislen] = i+1;
              INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
              ++thislen;
            }
          } else if (k < to[i]) {
            j=0; m=0;
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(lookup, to[i]-1);
            while (j<count[k-1] && m<count[to[i]-1]) {
              if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
                INTEGER(f1__)[thislen] = i+1;
                INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
                 ++thislen; ++j; ++m;
               } else if ( INTEGER(tmp1)[j] > INTEGER(tmp2)[m] ) {
                 ++m;
               } else ++j;
             }
           }
         }
         if (len == thislen) {
           INTEGER(f1__)[thislen] = i+1;
           INTEGER(f2__)[thislen] = nomatch;
           ++thislen;
         }
      }
      break;
    default: error(_("Internal error: unknown type in mult=%d in overlaps: %d"), mult, type); // #nocov
    }
    break;

  case FIRST:
    switch (type) {
    case START: case END:
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        k = (from[i]>0) ? from[i] : 1;
        if (k <= to[i]) { // count[k-1] is equal to type_count[k-1] and will always be >0, so no length check necessary.
          tmp1 = VECTOR_ELT(lookup, k-1);
          INTEGER(f2__)[thislen] = INTEGER(tmp1)[0];
          ++thislen;
        }
        if (len == thislen) {
          INTEGER(f2__)[thislen] = nomatch;
          ++thislen;
        }
      }
      break;

    case EQUAL :
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        if (from[i] > 0 && to[i] > 0) {
          k = from[i];
          if (k == to[i]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            INTEGER(f2__)[thislen] = INTEGER(tmp1)[0];
            ++thislen;
          } else if (k < to[i]) {
            j=0; m=0;
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(type_lookup, to[i]-1);
            while (j<count[k-1] && m<type_count[to[i]-1]) {
              if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
                INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
                 ++thislen; ++j; ++m;
                 break;
               } else if ( INTEGER(tmp1)[j] > INTEGER(tmp2)[m] ) {
                 ++m;
               } else ++j;
             }
           }
         }
         if (len == thislen) {
           INTEGER(f2__)[thislen] = nomatch;
           ++thislen;
         }
      }
      break;

    case ANY:
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        // k = (from[i]>0) ? from[i] : 1;
        k = from[i];
        for (j=k; j<=to[i]; j++) {
          if (type_count[j-1]) {
            tmp2 = VECTOR_ELT(type_lookup, j-1);
            INTEGER(f2__)[thislen] = INTEGER(tmp2)[0];
            ++thislen;
            break;
          }
        }
        if (len == thislen) {
          INTEGER(f2__)[thislen] = nomatch;
          ++thislen;
        }
      }
      break;

    case WITHIN:
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        k = from[i];
        if (k > 0) {
          if (k == to[i] && count[k-1]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            INTEGER(f2__)[thislen] = INTEGER(tmp1)[0];
            ++thislen;
          } else if (k < to[i]) {
            j=0; m=0;
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(lookup, to[i]-1);
            while (j<count[k-1] && m<count[to[i]-1]) {
              if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
                INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
                 ++thislen; ++j; ++m;
                 break;
               } else if ( INTEGER(tmp1)[j] > INTEGER(tmp2)[m] ) {
                 ++m;;
               } else ++j;
             }
           }
        }
        if (len == thislen) {
          INTEGER(f2__)[thislen] = nomatch;
          ++thislen;
        }
      }
      break;
    default: error(_("Internal error: unknown type in mult=%d in overlaps: %d"), mult, type); // #nocov
    }
    break;

  case LAST:
    switch (type) {
    case START: case END:
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        k = (from[i]>0) ? from[i] : 1;
        if (k <= to[i]) { // count[k-1] is equal to type_count[k-1] and will always be >0, so no length check necessary.
          tmp1 = VECTOR_ELT(lookup, k-1);
          INTEGER(f2__)[thislen] = INTEGER(tmp1)[count[k-1]-1];
          ++thislen;
        }
        if (len == thislen) {
          INTEGER(f2__)[thislen] = nomatch;
          ++thislen;
        }
      }
      break;

    case EQUAL :
      // Debugging reference for future-me
      // R -d lldb
      // run -f file.R
      // breakpoint set -f ijoin.c -l 591
      // c (hit enter to break at line 591)
      // n (next line)
      // p val # for native C objects
      // call Rf_PrintValue(val) # for SEXP objects, to print whole vector/vals
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        if (from[i] > 0 && to[i] > 0) {
          k = from[i];
          if (k == to[i]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            INTEGER(f2__)[thislen] = INTEGER(tmp1)[count[k-1]-1];
            ++thislen;
          } else if (k < to[i]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(type_lookup, to[i]-1);
            j=count[k-1]-1; m=type_count[to[i]-1]-1; // bug fix, k=from[i] but should be to[i]
            while (j>=0 && m>=0) {
              if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
                INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
                 ++thislen; --j; --m;
                 break;
               } else if ( INTEGER(tmp1)[j] < INTEGER(tmp2)[m] ) {
                 --m;
               } else --j;
             }
           }
         }
         if (len == thislen) {
           INTEGER(f2__)[thislen] = nomatch;
           ++thislen;
         }
      }
      break;

      // OLD logic for 'any,last' which had to check for maximum for each 'i'. Better logic below.
      // for 'first' we need to just get the minimum of first non-zero-length element, but not the same case for 'last'.
      // We've to loop over from[i]:to[i] and get maximum of all tmp2 values (each is of length 1 already conveniently set uo) in that range
      // case ANY:
      // for (i=0; i<rows; i++) {
      //     len = thislen;
      //     INTEGER(f1__)[thislen] = i+1;
      //     INTEGER(f2__)[thislen] = 0;
      //     // k = (from[i]>0) ? from[i] : 1;
      //     k = from[i];
      //     for (j=k; j<=to[i]; j++) {
      //         if (type_count[j-1]) {
      //             tmp2 = VECTOR_ELT(type_lookup, j-1);
      //             INTEGER(f2__)[thislen] = (INTEGER(f2__)[thislen] < INTEGER(tmp2)[type_count[j-1]-1]) ? INTEGER(tmp2)[type_count[j-1]-1] : INTEGER(f2__)[thislen];
      //         }
      //     }
      //     if (INTEGER(f2__)[thislen] == 0)
      //         INTEGER(f2__)[thislen] = nomatch;
      //     ++thislen;
      // }
      // break;

    case ANY:
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        // k = (from[i]>0) ? from[i] : 1;
        k = from[i];
        if (k <= to[i]) {
          if (k==to[i] && count[k-1]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            INTEGER(f2__)[thislen] = INTEGER(tmp1)[count[k-1]-1];
            ++thislen;
          } else {
            for (j=to[i]; j>k; j--) {
              if (type_count[j-1]) {
                tmp2 = VECTOR_ELT(type_lookup, j-1);
                INTEGER(f2__)[thislen] = INTEGER(tmp2)[0]; // tmp2 will be length 1
                ++thislen; break;
              }
            }
            if (len == thislen && count[k-1]) {
              tmp1 = VECTOR_ELT(lookup, k-1);
              INTEGER(f2__)[thislen] = INTEGER(tmp1)[count[k-1]-1];
              ++thislen;
            }
          }
        }
        if (len == thislen) {
          INTEGER(f2__)[thislen] = nomatch;
          ++thislen;
        }
      }
      break;

    case WITHIN:
      for (i=0; i<rows; i++) {
        len = thislen;
        INTEGER(f1__)[thislen] = i+1;
        k = from[i];
        if (k > 0) {
          if (k == to[i] && count[k-1]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            INTEGER(f2__)[thislen] = INTEGER(tmp1)[count[k-1]-1];
            ++thislen;
          } else if (k < to[i]) {
            tmp1 = VECTOR_ELT(lookup, k-1);
            tmp2 = VECTOR_ELT(lookup, to[i]-1);
            j=count[k-1]-1; m=count[to[i]-1]-1;
            while (j>=0 && m>=0) {
              if ( INTEGER(tmp1)[j] == INTEGER(tmp2)[m] ) {
                INTEGER(f2__)[thislen] = INTEGER(tmp1)[j];
                 ++thislen; --j; --m;
                 break;
               } else if ( INTEGER(tmp1)[j] < INTEGER(tmp2)[m] ) {
                 --m;
               } else --j;
             }
           }
         }
         if (len == thislen) {
           INTEGER(f2__)[thislen] = nomatch;
           ++thislen;
         }
      }
      break;
    default: error(_("Internal error: unknown type in mult=%d in overlaps: %d"), mult, type); // #nocov
    }
    break;
  default: error(_("Internal error: unknown mult in overlaps: %d"), mult); // #nocov
  }
  end2 = clock() - start;
  if (LOGICAL(verbose)[0])
    Rprintf(_("Final step, fetching indices in overlaps ... done in %8.3f seconds\n"), 1.0*(end2)/CLOCKS_PER_SEC);
  UNPROTECT(1);
  return(ans);
}
