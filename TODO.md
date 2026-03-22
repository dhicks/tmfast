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

### `ndR()` — missing `dplyr::` and `rlang::` namespace prefixes (fixed)
Two bare function calls in `R/information_gain.R` caused `ndR()` to fail when
called from a package context (functions not in scope without `dplyr` attached):
- Line 91: `pull(...)` → fixed to `dplyr::pull(...)`
- Lines 106–107: `enquo(doc_col)` / `enquo(term_col)` → fixed to `rlang::enquo(...)`

### `ndR()` — hardcoded column name `n` in totals calculation
In `R/information_gain.R` line 102, the totals summarize uses `sum(n)` instead
of `sum({{ count_col }})`. This means `ndR()` only works when the count column
is literally named `n`. Should be `dplyr::summarize(n_tot = sum({{ count_col }}))`.

---

## Test coverage gaps

### Remaining untested / placeholder

| Area | File | Notes |
|------|------|-------|
| `hellinger.data.frame()` | `test-hellinger.R` | Explicit "Not yet tested" placeholder |
| `tsne.*()` | `test-hellinger.R` | Explicit "Not yet tested" placeholder |
| `umap.*()` | — | No tests at all |
| `information_gain.R` | — | Zero tests: `entropy`, `ndH`, `ndR`, `expected_entropy`, `solve_power`, `target_power`, `renorm` |
| `journal_specific()` | — | Complex simulation; no tests |
| `varimax_irlba()` | `test-tmfast.R` | Stub warning only ("Rely on tests in irlba") |

### Done this session
- `tmfast()` — 4 tests
- `insert_topics()` — 5 tests (guards + structure; scores unverified due to bug above)
- `tidy.tmfast()` beta + gamma — 5 tests each
- `tidy_all()` — 3 tests
- `loadings()`, `scores()`, `rotation()` return values — 3 tests
- Bug fix: `insert_topics()` variable name typo (`fitted_tmf` → `fitted`)
