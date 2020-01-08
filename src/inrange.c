#include "data.table.h"
#include <Rdefines.h>


SEXP inrange(SEXP ansArg, SEXP xoArg, SEXP startsArg, SEXP lenArg) {

  int *ans = INTEGER(ansArg), *xo = INTEGER(xoArg);
  int *starts = INTEGER(startsArg), *len = INTEGER(lenArg);
  R_len_t i, j, n = length(startsArg), nxo = length(xoArg);
  for (i = 0; i < n; i++) {
    for (j = starts[i]-1; j < starts[i]-1+len[i]; j++) {
      ans[nxo ? xo[j]-1 : j] = 1;
    }
  }
  // old complicated logic which is only really useful when matches
  // contains A LOT of overlapping indices.. rare in real examples.
  // so switched to simpler logic above.. retaining it commented for now.

  // R_len_t i =0,j, ss,ee,new_ss,new_ee;
  // while(i < n && starts[i] == 0) i++;
  // while (i < n) {
  //     ss = starts[i]-1;
  //     ee = ss + len[i]-1;
  //     // Rprintf(_("Starting at %d, start=%d, end=%d\n"), i, ss, ee);
  //     // ss[i+1] >= ss[i] due to ordering from R-side
  //     // if ee[i] >= ss[i+1], then there's overlap, pick largest of ee[i], ee[i+1]
  //     while(++i < n && ee >= (new_ss = starts[i]-1)) {
  //         new_ee = new_ss + len[i]-1;
  //         ee = ee > new_ee ? ee : new_ee;
  //     }
  //     // Rprintf(_("Moved to %d, start=%d, end=%d\n"), i, ss, ee);
  //     for (j=ss; j<=ee; j++) ans[nxo ? xo[j]-1 : j] = 1;
  // }
  return (R_NilValue);
}
