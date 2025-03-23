#include <pthread.h>

#include "data.table.h"

struct hash_pair {
    SEXP key;
    R_xlen_t value;
};
struct hash_tab {
  size_t size, free;
  uintptr_t multiplier1, multiplier2;
  struct hash_pair *tb1, *tb2;
};

// TAOCP vol. 3, section 6.4: for multiplication hashing, use A ~ 1/phi, the golden ratio.
static const double hash_multiplier1 = 0.618033988749895;
static const double hash_multiplier2 = 0.316227766016838;

static R_INLINE size_t get_full_size(size_t n_elements, double load_factor) {
  if (load_factor <= 0 || load_factor >= 1)
    internal_error(__func__, "load_factor=%g not in (0, 1)", load_factor); // # nocov
  // precondition: n / load_factor < SIZE_MAX
  // this is implemented a bit stricter than needed and would fail some almost-too-high sizes
  // due to the size_t -> double conversion
  if ((size_t)((double)SIZE_MAX * load_factor) <= n_elements) internal_error(
    __func__, "n=%zu / load_factor=%g would overflow size_t",
    n_elements, load_factor
  );
  return ceil(n_elements / load_factor);
}

static hashtab * hash_create_(size_t n, double load_factor) {
  size_t n_full = get_full_size(n, load_factor);
  // precondition: sizeof hashtab + hash_pair[n_full] < SIZE_MAX
  //                        n_full * sizeof hash_pair < SIZE_MAX - sizeof hashtab
  //                                 sizeof hash_pair < (SIZE_MAX - sizeof hashtab) / n_full
  // (note that sometimes n is 0)
  if (n_full && sizeof(struct hash_pair) >= (SIZE_MAX - sizeof(hashtab)) / n_full)
    internal_error(
      __func__, "n=%zu with load_factor=%g would overflow total allocation size",
      n, load_factor
    );
  hashtab *ret = (hashtab *)R_alloc(sizeof(hashtab), 1);
  ret->size = n_full;
  ret->free = n;
  // To compute floor(size * (A * key % 1)) in integer arithmetic with A < 1, use ((size * A) * key) % size.
  ret->multiplier1 = n_full * hash_multiplier1;
  ret->multiplier2 = n_full * hash_multiplier2;
  ret->tb1 = (struct hash_pair *)R_alloc(sizeof(struct hash_pair[n_full]), 1);
  ret->tb2 = (struct hash_pair *)R_alloc(sizeof(struct hash_pair[n_full]), 1);
  // No valid SEXP is a null pointer, so it's a safe marker for empty cells.
  for (size_t i = 0; i < n_full; ++i) {
    ret->tb1[i].key = NULL;
    ret->tb2[i].key = NULL;
  }
  return ret;
}

hashtab * hash_create(size_t n) { return hash_create_(n, .5); }

// Hashing for an open addressing hash table. See Cormen et al., Introduction to Algorithms, 3rd ed., section 11.4.
// This is far from perfect. Make size a prime or a power of two and you'll be able to use double hashing.
static R_INLINE size_t hash_index1(SEXP key, uintptr_t multiplier) {
  // The 4 lowest bits of the pointer are probably zeroes because a typical SEXPREC exceeds 16 bytes in size.
  // Since SEXPRECs are heap-allocated, they are subject to malloc() alignment guarantees,
  // which is at least 4 bytes on 32-bit platforms, most likely more than 8 bytes.
  return ((((uintptr_t)key) >> 4) & 0x0fffffff) * multiplier;
}

static R_INLINE size_t hash_index2(SEXP key, uintptr_t multiplier) {
    return ((((uintptr_t)key) >> 6) & 0x0fffffff) * multiplier;
}


void hash_set(hashtab *h, SEXP key, R_xlen_t value) {
  size_t max_relocations = h->size; 
  struct hash_pair item = { .key = key, .value = value };
  for (size_t i = 0; i < max_relocations; ++i) {
    size_t idx1 = hash_index1(item.key, h->multiplier1) % h->size;
    if (!h->tb1[idx1].key) {
      h->tb1[idx1] = item;
      return;
    }
    struct hash_pair temp = h->tb1[idx1];
    h->tb1[idx1] = item;
    item = temp;    
    
    size_t idx2 = hash_index2(item.key, h->multiplier2) % h->size;
    if (!h->tb2[idx2].key) {
      h->tb2[idx2] = item;
      return;
    }
    temp = h->tb2[idx2];
    h->tb2[idx2] = item;
    item = temp;
  }
  internal_error(__func__, "Cuckoo hashing cycle detected, rehash needed");
}

R_xlen_t hash_lookup(const hashtab *h, SEXP key, R_xlen_t ifnotfound) {
  size_t idx1 = hash_index1(key, h->multiplier1) % h->size;
  if (h->tb1[idx1].key == key) return h->tb1[idx1].value;
    
  size_t idx2 = hash_index2(key, h->multiplier2) % h->size;
  if (h->tb2[idx2].key == key) return h->tb2[idx2].value;
  // Should be impossible with a load factor below 1, but just in case:
  return ifnotfound; // # nocov
}

typedef struct dhashtab_ {
  dhashtab public; // must be at offset 0
  size_t size, used, limit;
  uintptr_t multiplier;
  struct hash_pair *table, *previous;
} dhashtab_;

static void dhash_finalizer(SEXP dhash) {
  dhashtab_ * self = R_ExternalPtrAddr(dhash);
  if (!self) return;
  R_ClearExternalPtr(dhash);
  free(self->previous);
  free(self->table);
  free(self);
}

static struct hash_pair * dhash_allocate(size_t n_full) {
  if (n_full > SIZE_MAX / sizeof(struct hash_pair))
    internal_error(__func__, "%zu hash table slots would overflow size_t", n_full); // # nocov
  struct hash_pair * new = malloc(sizeof(struct hash_pair[n_full]));
  if (!new) internal_error(__func__, "failed to malloc() %zu hash table slots", n_full); // # nocov
  for (size_t i = 0; i < n_full; ++i) new[i] = (struct hash_pair){.key = NULL};
  return new;
}

static dhashtab * dhash_create_(size_t n, double load_factor) {
  size_t n_full = get_full_size(n, load_factor);

  SEXP prot = PROTECT(R_MakeExternalPtr(NULL, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(prot, dhash_finalizer, TRUE);
  dhashtab_ * self = malloc(sizeof(dhashtab_));
  if (!self) internal_error(__func__, "failed to malloc() the hash table header"); // # nocov
  *self = (dhashtab_){
    .public = { .prot = prot },
  };
  R_SetExternalPtrAddr(prot, self);

  self->table = dhash_allocate(n_full);
  self->size = n_full;
  self->limit = n;
  self->multiplier = n_full * hash_multiplier1;
  // this is the last time we're allowed to set the table parts piece by piece

  UNPROTECT(1);
  return &self->public;
}

dhashtab * dhash_create(size_t n) { return dhash_create_(n, .5); }

static void dhash_enlarge(dhashtab_ * self) {
  if (self->size > SIZE_MAX / 2)
    internal_error(__func__, "doubling %zu elements would overflow size_t", self->size); // # nocov
  size_t new_size = self->size * 2;
  struct hash_pair * new = dhash_allocate(new_size);
  uintptr_t new_multiplier = new_size * hash_multiplier1;
  for (size_t i = 0; i < self->size; ++i) {
    for (size_t j = 0; j < new_size; ++j) {
      size_t ii = (hash_index1(self->table[i].key, new_multiplier) + j) % new_size;
      if (!new[ii].key) {
        new[ii] = (struct hash_pair){
          .key = self->table[i].key,
          .value = self->table[i].value
        };
        break;
      }
    }
  }
  // Not trying to protect from calls to _set -> _enlarge from other threads!
  // Writes only come from a critical section, so two threads will not attempt to enlarge at the same time.
  // What we have to prevent is yanking the self->table from under a different thread reading it right now.
  free(self->previous);
  struct hash_pair * previous = self->table;
  dhashtab public = self->public;
  size_t used = self->used, limit = self->limit*2;
  *self = (dhashtab_){
    .public = public,
    .size = new_size,
    .used = used,
    .limit = limit,
    .multiplier = new_multiplier,
    .table = new,
    .previous = previous,
  };
  #pragma omp flush // no locking or atomic access! this is bad
}

void dhash_set(dhashtab * h, SEXP key, R_xlen_t value) {
  dhashtab_ * self = (dhashtab_ *)h;
  struct hash_pair *cell, *end;
again:
  cell = self->table + hash_index1(key, self->multiplier) % self->size;
  end = self->table + self->size - 1;
  for (size_t i = 0; i < self->size; ++i, cell = cell == end ? self->table : cell+1) {
    if (cell->key == key) {
      cell->value = value;
      return;
    } else if (!cell->key) {
      if (self->used < self->limit) {
        *cell = (struct hash_pair){ .key = key, .value = value };
        ++self->used;
        return;
      }
      dhash_enlarge(self);
      goto again; // won't be needed next time with the limit doubled
    }
  }
  internal_error( // # nocov
    __func__, "did not find a free slot for key %p; size=%zu, used=%zu, limit=%zu",
    (void*)key, self->size, self->used, self->limit
  );
}

R_xlen_t dhash_lookup(dhashtab * h, SEXP key, R_xlen_t ifnotfound) {
  #pragma omp flush // no locking or atomic access! this is bad
  dhashtab_ self = *(dhashtab_ *)h;
  R_xlen_t ret = ifnotfound;
  const struct hash_pair * cell = self.table + hash_index1(key, self.multiplier) % self.size;
  const struct hash_pair * end = self.table + self.size - 1;
  for (size_t i = 0; i < self.size; ++i, cell = cell == end ? self.table : cell+1) {
    if (cell->key == key) {
      ret = cell->value;
      goto done;
    } else if (!cell->key) {
      goto done;
    }
  }
done:
  return ret;
}
