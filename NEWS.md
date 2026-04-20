# tmfast 0.0.0.2026-04-20

## Breaking changes

* `tidy.tmfast()` now uses softmax to convert varimax loadings and scores into
  probability distributions, replacing the previous trim-and-normalize (beta) and
  nudge-and-normalize (gamma) approaches. Beta distributions now include all tokens
  (previously, tokens with negative loadings were silently dropped).
