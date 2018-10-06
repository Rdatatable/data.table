#include "memTrack.h"

void*** initMemTracker(int n){
  // args:
  //   n determines the maximum number of trackable allocs
  // return:
  //   void*** pointer with 2*n + 3 slots of pointers to pointers. Assuming the return is called r and we have n = 10, 
  //   the slot layout is as follows:
  //   r[0] = (int**) stores the number n of trackable allocs.
  //   r[1] = (int**) number of registered pointers from malloc or calloc calls that need to be cleared with free() at exist. Always <=n
  //   r[2:11] = (void**) pointers to the pointers that need to be free()d because they were malloc/calloc ed
  //   r[12] = (int**)  number of registered pointers from R's Calloc calls that need to be cleared with R's Free() at exist. Always <=n
  //   r[13:22] = (void**) pointers to the pointers that need to be Free() ed because they were Calloc ed
  
  // allocate tracker array. Size of all pointers is equal
  void ***out = malloc((2 * n + 3) * sizeof(void**));
  if (!out) 
    error("Failed to allocate memory for memory tracker");
  // set the total number of storable pointers
  out[0] = malloc(sizeof(int));
  if (!out[0]){
    free(out);
    error("Failed to allocate memory for memory tracker");
  }
  *((int*)out[0]) = n;
  // initialize stored pointer counters to 0 with calloc
  out[1] = calloc(1, sizeof(int));
  if (!out[1]){
    free(out[0]);
    free(out);
    error("Failed to allocate memory for memory tracker");
  }
  out[n + 2] = calloc(1, sizeof(int));
  if (!out[n + 2]){
    free(out[0]);
    free(out[1]);
    free(out);
    error("Failed to allocate memory for memory tracker");
  }
  // initialize the slots for stored pointers with NULL pointers
  for (int i = 0; i < n; i++){
    out[i + 2] = NULL;
    out[i + n + 3] = NULL;
  }
  return(out);
}

static void deleteMemTracker(void** memTracker){
  // free all the assigned slots in memTracker object
  int n = *((int*)memTracker[0]);
  free(memTracker[0]);
  free(memTracker[1]);
  free(memTracker[n + 2]);
  free(memTracker);
}

void freeAll(void** memTracker){
  // frees all the stored pointers in memTracker and finally memTracker itself
  int n = *((int*)memTracker[0]); // total number of slots
  int n_malloc = *((int*)memTracker[1]); // number of used slots in c-style malloc / calloc
  int n_ralloc = *((int*)memTracker[n + 2]); // number of used slots in r-style Calloc
  // free c-style allocated first
  for (int i = 0; i < n_malloc; i++){
    free(memTracker[i + 2]);
    memTracker[i+2] = NULL;
  }
  // now, free r style Callocated
  for (int i = 0; i < n_ralloc; i++){
    Free(memTracker[i + n + 3]); // no need to set to NULL because Free does that according to R manual
  }
  deleteMemTracker(memTracker);
}

