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

**Approach**: standalone data package `tmfast.realbooks` (lives in `tmfast_realbooks/`
subfolder). `corpus_raw` (33.7 MB xz) is the pre-download corpus (before `unnest_tokens`).
The data package passes `R CMD check` cleanly. Distribution via GitHub + drat:
https://blog.thecoatlessprofessor.com/programming/r/r-data-packages-in-external-data-repositories-using-the-additional-repositories-field/index.html

**Part 1 done**: `tmfast_realbooks/` created, `corpus_raw.rda` saved, `devtools::check()` clean.

**Part 2 done**: `vignettes/realbooks.Rmd` rewritten — corpus loaded from `tmfast.realbooks`,
`knitr::opts_chunk$set(eval = requireNamespace(...))` guards all chunks, vignette knits clean.

**Remaining (deferred until data package is published)**:
- Use drat to publish data package: <https://eddelbuettel.github.io/drat/vignettes/dratstepbystep/>
- Update `tmfast/DESCRIPTION`:
  - Add `tmfast.realbooks` to `Suggests`
  - Add `Additional_repositories` pointing to drat/r-universe URL
  - Remove `gutenbergr` and `memoise` from `Suggests`
- update `realbooks.Rmd` with correct URLs and installation instructions
- Optional: add `.onLoad()` hook in `tmfast` to register external repo on package load (cf <https://blog.thecoatlessprofessor.com/programming/r/r-data-packages-in-external-data-repositories-using-the-additional-repositories-field/index.html>)

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
