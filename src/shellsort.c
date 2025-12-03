#include "data.table.h"

/* shellsort's gap sequences
# Sedgewick 1982
sedgewick1982 = function(k) 4^k+3*2^(k-1)+1
# Sedgewick 1986
sedgewick1986 = function(k) if (k%%2) 8*2^k-6*2^((k+1)/2)+1 else 9*(2^k-2^(k/2))+1
# Tokuda 1992
tokuda = function(k, y=2.25) ceiling((y^k-1)/(y-1))
# Ciura 2001
ciura2001 = function(k) if (k==1) 1 else if (k==2) 4 else if (k==3) 10 else if (k==4) 23 else if (k==5) 57 else if (k==6) 132 else if (k==7) 301 else if (k==8) 701 else floor(2.25 * ciura(k-1))
# Lee 2021
lee = function(k, y=2.243609061420001) tokuda(k, y=y)
*/
static const int sedgewick1982[17] = {
  1073790977, 268460033, 67121153, 16783361, 4197377, 1050113,
  262913, 65921, 16577, 4193, 1073, 281, 77, 23, 8, 1, 0
};
#define NGAPS 16

/* shellsort
 * uses sedgewick1982 gap sequence formula as it turned out to be the most efficient - tested various n, k (for rnorm only!)
 * currently used in frollmedian algo="fast" when no NAs
*/
void shellsort(const double *x, int n, int *o) {
  for (int i=0; i < n; i++) o[i] = i;
  int gap = 0;
  while (sedgewick1982[gap] > n) gap++;
  for (int h=sedgewick1982[gap]; gap<NGAPS; h=sedgewick1982[++gap]) {
    for (int i=h; i<n; i++) {
      int io = o[i];
      int j = i;
      while (j >= h && (x[o[j-h]] > x[io] || (x[o[j-h]] == x[io] && /*this makes stable sort*/ o[j-h] > io))) {
        o[j] = o[j-h];
        j -= h;
      }
      o[j] = io;
    }
  }
}

/* shellsort with NA support
 * we support NAs by replacing them by Inf, so they are at the end
 * to still support Inf in the data we use isna mask to check if Inf was really Inf or it was NA
 *
 * not used anymore as frollmedian algo="exact" switched from shellshort to quickselect
 * could be used when algo="fast" will have NA support
 *
 * NAs has to be decoded to Inf inside x
 * isna mask must be computed
*/
// void shellsortna(double *x, int n, int *o, bool *isna) {
//   for (int i=0; i<n; i++) o[i] = i;
//   int gap = 0;
//   while (sedgewick1982[gap] > n) gap++;
//   for (int h=sedgewick1982[gap]; gap<NGAPS; h=sedgewick1982[++gap]) {
//     for (int i=h; i<n; i++) {
//       int io = o[i];
//       int j = i;
//       while (j >= h && (
//         (x[o[j-h]] > x[io]) || (
//             (x[o[j-h]] == x[io] && (/*both not NA*/(!isna[o[j-h]] && !isna[io]) || /*both NA*/(isna[o[j-h]] && isna[io])) && /*this makes stable sort*/ o[j-h] > io)
//                || (/*NA and real R_PosInf*/x[o[j-h]] == x[io] && isna[o[j-h]] && !isna[io])
//         ))) {
//         o[j] = o[j-h];
//         j -= h;
//       }
//       o[j] = io;
//     }
//   }
// }
