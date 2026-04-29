# TODO

- exclude `TODO.md` from `pkgdown`
    - see <https://github.com/r-lib/pkgdown/issues/2959>

## Vignettes

- update arXiv paper for softmax


## CRAN blockers
---

## Documentation issues
(`devtools::check()` as of 2026-03-24)

### WARNINGs
- S3 generic/method consistency: `hellinger`, `umap`, and `tsne` generics use `function(x,
  ...)` but methods use different first-argument names (`topicsdf1`, `mx1`, `dist_mx`,
  `model`, `tm`). Fix: rename the first argument in each method to `x`.

