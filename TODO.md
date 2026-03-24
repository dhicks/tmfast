# TODO

## Bugs

### `fit_varimax()` — duck-typed `pca` argument
`fit_varimax()` accesses `pca$rotation`, `pca$sdev`, and `pca$x` by name, accepting either
a `prcomp_irlba` result or a `varimaxes` object by coincidence of shared field names. The
original `insert_topics()` bug was a duck-typing failure of exactly this kind. The `@param
pca` roxygen entry should document exactly which fields are accessed, to prevent future
callers from accidentally passing an incompatible object.

---

## CRAN blockers

### Vignette network dependency — `vignettes/realbooks.Rmd`
`R CMD build` re-knits all vignettes in a clean temp directory. `realbooks.Rmd` requires
network access (Project Gutenberg) and a memoise cache, so it always fails in that context.
The vignette builds fine locally with the cache present. Options:

- **R.rsp asis engine**: add `R.rsp` to Suggests, provide the pre-built HTML as a `.html`
  vignette with an asis header — R CMD build copies it without re-knitting. Most compatible
  with CRAN.
- **Cache data in `inst/extdata/`**: store the downloaded Gutenberg data as `.rda` and load
  from there, removing the network dependency entirely.
- **`eval=FALSE` stub**: replace live chunks with pre-computed inline output. Significant
  rewrite.

---

## Documentation issues
(`devtools::check()` as of 2026-03-24)

### WARNINGs
- `tsne.data.frame.Rd`: documented arguments `tm` and `k` are not in `\usage`. They belong
  to `tsne.tmfast`, not `tsne.data.frame` — the roxygen block on `tsne.tmfast` at
  [R/space.R:56](R/space.R#L56) is accidentally merged into the `.data.frame` Rd. Needs a
  separate `#'` block for `tsne.data.frame`.
- S3 generic/method consistency: `hellinger`, `umap`, and `tsne` generics use `function(x,
  ...)` but methods use different first-argument names (`topicsdf1`, `mx1`, `dist_mx`,
  `model`, `tm`). Fix: rename the first argument in each method to `x`.

### NOTEs
- Many "no visible binding" warnings (`dH`, `doc`, `document`, `n`, `p`, etc.) — tidy eval
  variables used bare inside `dplyr` verbs. Fix with `utils::globalVariables()` declaration
  or `.data$` pronoun.
- `TODO.md` at top level is non-standard. Can move to `inst/` or add to `.Rbuildignore`.
- Some exported functions have incomplete or missing documentation; internal functions could
  use comments for future developers.
