#include "data.table.h"

#define HASH_BITS (8 * sizeof(void*))
#define HASH_MULTIPLIER ((sizeof(void*) == 8) ? 11400714819323198485ULL : 2654435769U)
struct hash_pair {
    SEXP key;
    R_xlen_t value;
};
struct hash_tab {
  size_t size, free;
  int shift;
  struct hash_pair *table;
};

static const double default_load_factor = .75;

static R_INLINE size_t get_next_pow2(size_t n) {
  if (n <= 1) return 1;
  n--;
  n |= n >> 1;
  n |= n >> 2;
  n |= n >> 4;
  n |= n >> 8;
  n |= n >> 16;
  if (sizeof(size_t) > 4) n |= n >> 32;
  return n + 1;
}

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
  size_t min_size = (size_t)(n_elements / load_factor) + 1;
  // Round up to next power of 2 for fast modulo using bitwise AND
  size_t pow2 = get_next_pow2(min_size);
  if (pow2 == 0) {
      internal_error(__func__, "size %zu would overflow size_t", min_size); // # nocov
  }
  return pow2;
}

static hashtab * hash_create_(size_t n, double load_factor) {
  size_t n_full = get_full_size(n, load_factor);
  hashtab *ret = (hashtab *)R_alloc(sizeof(hashtab), 1);
  ret->size = n_full;
  ret->free = (size_t)(n_full * load_factor);

  int k = 0;
  while ((1ULL << k) < n_full) k++;
  ret->shift = HASH_BITS - k;
  ret->table = (struct hash_pair *)R_alloc(n_full, sizeof(*ret->table));
  // No valid SEXP is a null pointer, so it's a safe marker for empty cells.
  memset(ret->table, 0, n_full * sizeof(struct hash_pair));
  return ret;
}

hashtab * hash_create(size_t n) { return hash_create_(n, default_load_factor); }

static R_INLINE size_t hash_index(SEXP key, int shift) {
  return (size_t)((uintptr_t)key * HASH_MULTIPLIER) >> shift;
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
  size_t idx = hash_index(key, h->shift);
  while (true) {
    if (!h->table[idx].key) {
      if (h->free == 0) return false; // table full -> need rehash
      h->table[idx].key = key;
      h->table[idx].value = value;
      h->free--;
      return true;
    }

    if (h->table[idx].key == key) {
      h->table[idx].value = value;
      return true;
    }
    idx = (idx + 1) & mask;
  }
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
  size_t idx = hash_index(key, h->shift);
  while (true) {
    if (h->table[idx].key == key) return h->table[idx].value;
    if (h->table[idx].key == NULL) return ifnotfound;
    idx = (idx + 1) & mask;
  }
}
