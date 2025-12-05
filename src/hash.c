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
  if (!hash_set_(h, key, value)) {
    *h = *hash_rehash(h);
    (void)hash_set_(h, key, value); // must succeed on the second try
  }
}

hashtab *hash_set_shared(hashtab *h, SEXP key, R_xlen_t value) {
  if (!hash_set_(h, key, value)) {
    h = hash_rehash(h);
    (void)hash_set_(h, key, value);
  }
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
