#ifndef MEMTRACK_H
#define MEMTRACK_H

#include<stdlib.h>
#include <R.h>

void*** initMemTracker(int n);

void* safe_malloc(size_t size, void*** memTracker);
void* safe_realloc(void *ptr, size_t new_size, void*** memTracker);
void* safe_calloc(size_t num, size_t size, void*** memTracker);

void freeAll(void*** memTracker);
void freeExceptReturn(void*** memTracker, void* returnValue);


#endif