#include "data.table.h"

#define HASH_BITS (8 * sizeof(void*))
#define HASH_MULTIPLIER ((sizeof(void*) == 8) ? 11400714819323198485ULL : 2654435769U)
struct hash_pair {
    SEXP key;
    R_xlen_t value;
};
struct hash_tab {
  hashtab public; // must be the first member
  size_t size, free;
  int shift;
  struct hash_pair *table;
  struct hash_tab *next;
};

static const double default_load_factor = .75;

static R_INLINE size_t get_next_pow2(size_t n) {
  if (n <= 2) return 2;
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

static struct hash_tab * hash_create_(size_t n, double load_factor) {
  size_t n_full = get_full_size(n, load_factor);

  struct hash_tab *ret = malloc(sizeof *ret);
  if (!ret)
    return NULL; // #nocov

  // Not strictly portable but faster than an explicit loop setting keys to NULL
  struct hash_pair *table = calloc(n_full, sizeof(*table));
  if (!table) {
    // #nocov start
    free(ret);
    return NULL;
    // #nocov end
  }

  ret->size = n_full;
  ret->free = (size_t)(n_full * load_factor);

  int k = 0;
  while ((1ULL << k) < n_full) k++;
  ret->shift = HASH_BITS - k;
  ret->table = table;
  ret->next = NULL;

  return ret;
}

static void hash_finalizer(SEXP prot) {
  struct hash_tab * h = R_ExternalPtrAddr(prot);
  R_ClearExternalPtr(prot);
  while (h) {
    struct hash_tab * next = h->next;
    free(h->table);
    free(h);
    h = next;
  }
}

hashtab * hash_create(size_t n) {
  SEXP prot = R_MakeExternalPtr(NULL, R_NilValue, R_NilValue);
  R_RegisterCFinalizer(prot, hash_finalizer);

  struct hash_tab *ret = hash_create_(n, default_load_factor);
  if (!ret) internal_error( // #nocov
    __func__, "failed to allocate a hash table for %zu entries", n
  );

  ret->public.prot = prot;
  R_SetExternalPtrAddr(prot, ret);

  return &ret->public;
}

static R_INLINE size_t hash_index(SEXP key, int shift) {
  return (size_t)((uintptr_t)key * HASH_MULTIPLIER) >> shift;
}

static bool hash_set_(struct hash_tab *h, SEXP key, R_xlen_t value) {
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

static R_INLINE struct hash_tab *hash_rehash(const struct hash_tab *h) {
  if (h->size > SIZE_MAX / 2) return NULL; // #nocov

  size_t new_size = h->size * 2;
  struct hash_tab *new_h = hash_create_(new_size, default_load_factor);
  if (!new_h) return NULL;

  new_h->public = h->public;

  for (size_t i = 0; i < h->size; ++i) {
    if (h->table[i].key)
      (void)hash_set_(new_h, h->table[i].key, h->table[i].value);
  }

  return new_h;
}

void hash_set(hashtab *self, SEXP key, R_xlen_t value) {
  struct hash_tab *h = (struct hash_tab *)self;
  if (!hash_set_(h, key, value)) {
    struct hash_tab *new_h = hash_rehash(h);
    if (!new_h) internal_error( // # nocov
      __func__, "hash table full at n_full=%zu and failed to rehash", h->size
    );
    // overwrite the existing table, keeping the EXTPTR -> (next ->)* h chain intact
    free(h->table);
    *h = *new_h;
    free(new_h);
    (void)hash_set_(h, key, value); // must succeed on the second try
  }
}

hashtab *hash_set_shared(hashtab *self, SEXP key, R_xlen_t value) {
  struct hash_tab *h = (struct hash_tab *)self;
  if (!hash_set_(h, key, value)) {
    struct hash_tab * new_h = hash_rehash(h);
    if (!new_h) return NULL; // # nocov
    // existing 'h' may still be in use by another thread
    h->next = new_h;
    h = new_h;
    (void)hash_set_(h, key, value);
  }
  return &h->public;
}

R_xlen_t hash_lookup(const hashtab *self, SEXP key, R_xlen_t ifnotfound) {
  const struct hash_tab *h = (const struct hash_tab *)self;

  size_t mask = h->size - 1;
  size_t idx = hash_index(key, h->shift);
  while (true) {
    if (h->table[idx].key == key) return h->table[idx].value;
    if (h->table[idx].key == NULL) return ifnotfound;
    idx = (idx + 1) & mask;
  }
}
