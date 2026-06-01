# tmfast 0.1.1.2026-06-01

* Added CRAN status badge to README.
* JOSS paper submission. 

# tmfast 0.1.1

* Initial CRAN publication

# tmfast 0.1.0

* Initial CRAN submission

# tmfast 0.0.0.2026-04-20

## Breaking changes

* `tidy.tmfast()` now uses softmax to convert varimax loadings and scores into
  probability distributions, replacing the previous trim-and-normalize (beta) and
  nudge-and-normalize (gamma) approaches. Beta distributions now include all tokens
  (previously, tokens with negative loadings were silently dropped).
