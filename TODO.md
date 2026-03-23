# TODO

## Bugs

### `insert_topics()` scores are computed incorrectly
When `insert_topics()` calls `fit_varimax()`, it passes the `tmfast`/`varimaxes`
object as the `pca` argument. `fit_varimax()` then uses `pca$x[, 1:k]` to compute
scores — but `$x` on a `varimaxes` object is the **original DTM** (stored when
`retx = TRUE`), not the PCA scores. The original `varimax_irlba()` call works
correctly because it passes the raw `prcomp_irlba` output (where `$x` *are* the
PCA scores). The fix likely requires either storing PCA scores separately in the
`varimaxes` object, or reprojecting the DTM through `fitted$rotation` inside
`insert_topics()` before passing to `fit_varimax()`.

### `umap.STM()` — missing `return()` statement
In [R/space.R:128](R/space.R#L128), `umap.STM()` assigns the result to `embedding` but
never returns it. The function silently returns `NULL`. Compare `umap.tmfast()` at line
112, which has `return(embedding)`. Fix: add `return(embedding)` as the last line.

### `ndH.ArrowObject()` — bare `enquo()` and hardcoded `n` in `ndH` column
In [R/information_gain.R:51](R/information_gain.R#L51), `enquo(term_col)` is called
without the `rlang::` prefix. The function may work if `rlang` is attached but will
fail in a clean package context. Fix: `rlang::enquo(term_col)`.

Additionally, line 47 uses `sum(n)` (hardcoded column name `n`) for `totals`, identical
to the known bug in `ndR()`. And line 58 references bare `n` instead of `{{ count_col
}}` — this only works when the count column is literally named `n`.

### `renormalize.R` — `solve_power()` risky condition accessing `$root` on `NA_real_`
In [R/renormalize.R:55](R/renormalize.R#L55), the condition is:
```r
if ((return_full || identical(soln, NA_real_)) || is.na(soln$root)) {
```
When `soln` is `NA_real_` (scalar), the short-circuit `||` *should* prevent evaluating
`is.na(soln$root)`, but operator precedence groups `(return_full || identical(...))` first,
so if `return_full` is `FALSE` and `soln` is `NA_real_`, `is.na(soln$root)` is still
reached and will error (`$` on an atomic). Fix: reorder as
`if (return_full || identical(soln, NA_real_) || is.na(soln$root))`.

### `hellinger.R` — deprecated `one_of()` in `pivot_longer()`
In [R/hellinger.R:137](R/hellinger.R#L137), `tidyr::pivot_longer(-one_of(id1), ...)`.
`one_of()` is deprecated in tidyselect; should be `all_of()` or `any_of()`.

### Documentation
Some exported functions have incomplete or no documentation. Internal functions don't need full documentation, but might need some comments explaining what they do, for future developers. 

---

## Test coverage gaps

| Area | File | Notes |
|------|------|-------|
| `varimax_irlba()` | `test-tmfast.R` | Stub only ("Rely on tests in irlba") |
| `draw_corpus()` | `test-generators.R` | No tests |
| `journal_specific()` | `test-generators.R` | No tests; complex simulation |
