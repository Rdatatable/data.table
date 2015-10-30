#include "data.table.h"
#include <Rdefines.h>
//#include <sys/mman.h>
#include <Rversion.h>
#include <fcntl.h>
#include <time.h>

// from good ol' Numerical Recipes in C
#define SWAP(a,b) temp=(a);(a)=(b);(b)=temp;

double dquickselect(double *x, int n, int k) {
    unsigned long i,ir,j,l,mid;
    double a,temp;

    l=0;
    ir=n-1;
    for(;;) {
        if (ir <= l+1) { 
            if (ir == l+1 && x[ir] < x[l]) {
                SWAP(x[l],x[ir]);
            }
        return x[k];
        } else {
            mid=(l+ir) >> 1; 
            SWAP(x[mid],x[l+1]);
            if (x[l] > x[ir]) {
                SWAP(x[l],x[ir]);
            }
            if (x[l+1] > x[ir]) {
                SWAP(x[l+1],x[ir]);
            }
            if (x[l] > x[l+1]) {
                SWAP(x[l],x[l+1]);
            }
            i=l+1; 
            j=ir;
            a=x[l+1]; 
            for (;;) { 
                do i++; while (x[i] < a); 
                do j--; while (x[j] > a); 
                if (j < i) break; 
                    SWAP(x[i],x[j]);
            } 
            x[l+1]=x[j]; 
            x[j]=a;
            if (j >= k) ir=j-1; 
            if (j <= k) l=i;
        }
    }
}

double iquickselect(int *x, int n, int k) {
    unsigned long i,ir,j,l,mid;
    int a,temp;

    l=0;
    ir=n-1;
    for(;;) {
        if (ir <= l+1) { 
            if (ir == l+1 && x[ir] < x[l]) {
                SWAP(x[l],x[ir]);
            }
        return (double)(x[k]);
        } else {
            mid=(l+ir) >> 1; 
            SWAP(x[mid],x[l+1]);
            if (x[l] > x[ir]) {
                SWAP(x[l],x[ir]);
            }
            if (x[l+1] > x[ir]) {
                SWAP(x[l+1],x[ir]);
            }
            if (x[l] > x[l+1]) {
                SWAP(x[l],x[l+1]);
            }
            i=l+1; 
            j=ir;
            a=x[l+1]; 
            for (;;) { 
                do i++; while (x[i] < a); 
                do j--; while (x[j] > a); 
                if (j < i) break; 
                    SWAP(x[i],x[j]);
            } 
            x[l+1]=x[j]; 
            x[j]=a;
            if (j >= k) ir=j-1; 
            if (j <= k) l=i;
        }
    }
}


// SEXP quickselect(SEXP xArg, SEXP n, SEXP k) {

//     void *x = DATAPTR(xArg);
//     SEXP ans = PROTECT(allocVector(REALSXP, 1L));
//     REAL(ans)[0] = quickselectwrapper(x, INTEGER(n)[0], INTEGER(k)[0]-1);

//     UNPROTECT(1);
//     return(ans);
// }
