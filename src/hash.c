#include "data.table.h"

struct hash_pair {
    SEXP key;
    R_xlen_t value;
};
struct hash_tab {
  size_t size, free;
  uintptr_t multiplier;
  struct hash_pair tb[];
};

hashtab * hash_create(size_t n) { return hash_create_(n, .5); }
// TAOCP vol. 3, section 6.4: for multiplication hashing, use A ~ 1/phi, the golden ratio.
static const double hash_multiplier = 0.618033988749895;

hashtab * hash_create_(size_t n, double load_factor) {
  if (load_factor <= 0 || load_factor >= 1)
    internal_error("hash_create", "load_factor=%g not in (0, 1)", load_factor);
  // precondition: n / load_factor < SIZE_MAX
  // truncate to compare in exact integer arithmetic and preserve all bits of n
  if ((size_t)(SIZE_MAX * load_factor) <= n) internal_error(
    "hash_create", "n=%zu / load_factor=%g would overflow size_t",
    n, load_factor
  );
  size_t n_full = ceil(n / load_factor);
  // precondition: sizeof hashtab + hash_pair[n_full] < SIZE_MAX
  //                        n_full * sizeof hash_pair < SIZE_MAX - sizeof hashtab
  //                                 sizeof hash_pair < (SIZE_MAX - sizeof hashtab) / n_full
  // (note that sometimes n is 0)
  if (n_full && sizeof(struct hash_pair) >= (SIZE_MAX - sizeof(hashtab)) / n_full) internal_error(
    "hash_create", "n=%zu with load_factor=%g would overflow total allocation size",
    n, load_factor
  );
  hashtab * ret = (hashtab *)R_alloc(sizeof(hashtab) + sizeof(struct hash_pair[n_full]), 1);
  ret->size = n_full;
  ret->free = n;
  // To compute floor(size * (A * key % 1)) in integer arithmetic with A < 1, use ((size * A) * key) % size.
  ret->multiplier = n_full * hash_multiplier;
  // No valid SEXP is a null pointer, so it's a safe marker for empty cells.
  for (size_t i = 0; i < n_full; ++i)
    ret->tb[i] = (struct hash_pair){.key = NULL, .value = 0};
  return ret;
}

// Hashing for an open addressing hash table. See Cormen et al., Introduction to Algorithms, 3rd ed., section 11.4.
// This is far from perfect. Make size a prime or a power of two and you'll be able to use double hashing.
static R_INLINE size_t hash_index(SEXP key, uintptr_t multiplier, size_t offset, size_t size) {
  // The 4 lowest bits of the pointer are probably zeroes because a typical SEXPREC exceeds 16 bytes in size.
  // Since SEXPRECs are heap-allocated, they are subject to malloc() alignment guarantees, which is at least 4 bytes on 32-bit platforms, most likely more than 8 bytes.
  return ((((uintptr_t)key) >> 4) * multiplier + offset) % size;
}

void hash_set(hashtab * h, SEXP key, R_xlen_t value) {
  for (size_t i = 0; i < h->size; ++i) {
    struct hash_pair * cell = h->tb + hash_index(key, h->multiplier, i, h->size);
    if (cell->key == key) {
      cell->value = value;
      return;
    } else if (!cell->key) {
      if (!h->free) internal_error(
        "hash_insert", "no free slots left (size=%zu after the load factor)", h->size
      );
      --h->free;
      *cell = (struct hash_pair){.key = key, .value = value};
      return;
    }
  }
  internal_error(
    "hash_insert", "did not find a free slot for key %p despite size=%zu, free=%zu",
    (void*)key, h->size, h->free
  );
}

R_xlen_t hash_lookup(const hashtab * h, SEXP key, R_xlen_t ifnotfound) {
  for (size_t i = 0; i < h->size; ++i) {
    const struct hash_pair * cell = h->tb + hash_index(key, h->multiplier, i, h->size);
    if (cell->key == key) {
      return cell->value;
    } else if (!cell->key) {
      return ifnotfound;
    }
  }
  // Should be impossible with a load factor below 1, but just in case:
  return ifnotfound;
}
