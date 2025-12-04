#include <pthread.h>

#include "data.table.h"

struct hash_pair {
    SEXP key;
    R_xlen_t value;
};
struct hash_tab {
  size_t size, free;
  uintptr_t multiplier1, multiplier2;
  struct hash_pair *table;  // Single table for double hashing
};

// 1/phi and sqrt(0.1)
static const double hash_multiplier1 = 0.618033988749895;
static const double hash_multiplier2 = 0.316227766016838;
static const double default_load_factor = .5;

static R_INLINE size_t get_full_size(size_t n_elements, double load_factor) {
  if (load_factor <= 0 || load_factor >= 1)
    internal_error(__func__, "load_factor=%g not in (0, 1)", load_factor); // # nocov
  // precondition: n / load_factor < SIZE_MAX
  // this is implemented a bit stricter than needed and would fail some almost-too-high sizes
  // due to the size_t -> double conversion
  if ((size_t)((double)SIZE_MAX * load_factor) <= n_elements) internal_error( // # nocov
    __func__, "n=%zu / load_factor=%g would overflow size_t",
    n_elements, load_factor
  );
  size_t min_size = ceil(n_elements / load_factor);
  // Round up to next power of 2 for fast modulo using bitwise AND
  size_t pow2 = 1;
  while (pow2 < min_size) {
    if (pow2 > SIZE_MAX / 2)
      internal_error(__func__, "size %zu would overflow size_t", min_size); // # nocov
    pow2 *= 2;
  }
  return pow2;
}

static hashtab * hash_create_(size_t n, double load_factor) {
  size_t n_full = get_full_size(n, load_factor);
  hashtab *ret = (hashtab *)R_alloc(sizeof(hashtab), 1);
  ret->size = n_full;
  ret->free = n;
  // Multiply by size to get different hash functions when rehashing
  ret->multiplier1 = n_full * hash_multiplier1;
  ret->multiplier2 = n_full * hash_multiplier2;
  ret->table = (struct hash_pair *)R_alloc(n_full, sizeof(*ret->table));
  // No valid SEXP is a null pointer, so it's a safe marker for empty cells.
  for (size_t i = 0; i < n_full; ++i) {
    ret->table[i].key = NULL;
  }
  return ret;
}

hashtab * hash_create(size_t n) { return hash_create_(n, default_load_factor); }

// double hashing
static R_INLINE size_t hash_index1(SEXP key, uintptr_t multiplier) {
  // The 4 lowest bits of the pointer are probably zeroes because a typical SEXPREC exceeds 16 bytes in size.
  // Since SEXPRECs are heap-allocated, they are subject to malloc() alignment guarantees,
  // which is at least 4 bytes on 32-bit platforms, most likely more than 8 bytes.
  return ((((uintptr_t)key) >> 4) & 0x0fffffff) * multiplier;
}

static R_INLINE size_t hash_index2(SEXP key, uintptr_t multiplier) {
  // For double hashing, we need a different hash that's coprime with table size.
  // We use higher-order bits that hash_index1 mostly ignores, and ensure
  // the result is always odd (coprime with power-of-2 table sizes).
  uintptr_t ptr = (uintptr_t)key;
  ptr = (ptr >> 12) | (ptr << (sizeof(uintptr_t) * 8 - 12));
  return ((ptr & 0x0fffffff) * multiplier) | 1;
}

static R_INLINE hashtab *hash_rehash(const hashtab *h) {
  size_t new_size = h->size * 2;
  hashtab *new_h = hash_create_(new_size, default_load_factor);

  for (size_t i = 0; i < h->size; ++i) {
    if (h->table[i].key) hash_set(new_h, h->table[i].key, h->table[i].value);
  }
  return new_h;
}

static bool hash_set_(hashtab *h, SEXP key, R_xlen_t value) {
  size_t mask = h->size - 1;
  size_t h1 = hash_index1(key, h->multiplier1) & mask;
  size_t h2 = hash_index2(key, h->multiplier2) & mask;

  if (h2 == 0) h2 = 1;
  else if ((h2 & 1) == 0) h2 |= 1;

  for (size_t i = 0; i < h->size; ++i) {
    size_t idx = (h1 + i * h2) & mask;

    if (!h->table[idx].key) {
      // Empty slot found
      h->table[idx].key = key;
      h->table[idx].value = value;
      h->free--;
      return true;
    }

    if (h->table[idx].key == key) {
      h->table[idx].value = value;
      return true;
    }
  }

  // need to rehash
  return false;
}

void hash_set(hashtab *h, SEXP key, R_xlen_t value) {
  if (!hash_set_(h, key, value))
    *h = *hash_rehash(h);
  (void)hash_set_(h, key, value); // must succeed on the second try
}

hashtab *hash_set_shared(hashtab *h, SEXP key, R_xlen_t value) {
  if (!hash_set_(h, key, value))
    h = hash_rehash(h);
  (void)hash_set_(h, key, value);
  return h;
}

R_xlen_t hash_lookup(const hashtab *h, SEXP key, R_xlen_t ifnotfound) {
  size_t mask = h->size - 1;
  size_t h1 = hash_index1(key, h->multiplier1) & mask;
  size_t h2 = hash_index2(key, h->multiplier2) & mask;

  if (h2 == 0) h2 = 1;
  else if ((h2 & 1) == 0) h2 |= 1;

  for (size_t i = 0; i < h->size; ++i) {
    size_t idx = (h1 + i * h2) & mask;
    if (!h->table[idx].key) return ifnotfound;
    if (h->table[idx].key == key) return h->table[idx].value;
  }

  return ifnotfound; // # nocov
}

typedef struct dhashtab_ {
  dhashtab public; // must be at offset 0
  size_t size, used, limit;
  uintptr_t multiplier;
  struct hash_pair *table;
} dhashtab_;

static void dhash_finalizer(SEXP dhash) {
  dhashtab_ * self = R_ExternalPtrAddr(dhash);
  if (!self) return;
  R_ClearExternalPtr(dhash);
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
  size_t new_mask = new_size - 1;
  struct hash_pair * new = dhash_allocate(new_size);
  uintptr_t new_multiplier = new_size * hash_multiplier1;
  for (size_t i = 0; i < self->size; ++i) {
    if (!self->table[i].key) continue;
    for (size_t j = 0; j < new_size; ++j) {
      size_t ii = (hash_index1(self->table[i].key, new_multiplier) + j) & new_mask;
      if (!new[ii].key) {
        new[ii] = (struct hash_pair){
          .key = self->table[i].key,
          .value = self->table[i].value
        };
        break;
      }
    }
  }
  // This is thread-unsafe, but this function is only called either from a single-thread context, or
  // from under an OpenMP critical section, so there's no reason to worry about another thread
  // getting a use-after-free. They are all sleeping.
  self->size = new_size;
  self->limit *= 2;
  self->multiplier = new_multiplier;
  self->table = new;
  #pragma omp flush
}

void dhash_set(dhashtab * h, SEXP key, R_xlen_t value) {
  dhashtab_ * self = (dhashtab_ *)h;
  struct hash_pair *cell, *end;
again:
  cell = self->table + (hash_index1(key, self->multiplier) & (self->size - 1));
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
  const struct hash_pair * cell = self.table + (hash_index1(key, self.multiplier) & (self.size - 1));
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
