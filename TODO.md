# TODO

## Bugs

### `fit_varimax()` — duck-typed `pca` argument
`fit_varimax()` accesses `pca$rotation`, `pca$sdev`, and `pca$x` by name, accepting either
a `prcomp_irlba` result or a `varimaxes` object by coincidence of shared field names. The
original `insert_topics()` bug was a duck-typing failure of exactly this kind. The `@param
pca` roxygen entry should document exactly which fields are accessed, to prevent future
callers from accidentally passing an incompatible object.

### Documentation
Some exported functions have incomplete or no documentation. Internal functions don't need full documentation, but might need some comments explaining what they do, for future developers. 

---

## Test coverage gaps

All resolved as of 2026-03-24.
