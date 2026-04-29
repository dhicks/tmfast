# TODO

- test coverage for `R/renormalize.R`

- exclude `TODO.md` from `pkgdown`
    - see <https://github.com/r-lib/pkgdown/issues/2959>

## Vignettes

- update arXiv paper


## CRAN blockers
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
- Some exported functions have incomplete or missing documentation; internal functions could
  use comments for future developers.
