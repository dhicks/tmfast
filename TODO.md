# TODO

- exclude `TODO.md` from `pkgdown`
    - see <https://github.com/r-lib/pkgdown/issues/2959>

## Vignettes

- update arXiv paper for softmax
    [Because the arXiv paper isn't included in the CRAN submission, skipping this for now]

## CRAN review


5. "Using foo:::f instead of foo::f allows access to unexported objects. This is generally not recommended, as the semantics of unexported objects may be changed by the package author in routine maintenance."

Please omit one colon.

-> Used ::: in documentation:
     man/hellinger.Rd:
        topics1 = tidyr::pivot_longer(dplyr::mutate(tibble::as_tibble(rdirichlet(3, rep(5, 5)), rownames = "doc_id", .name_repair = tmfast:::make_colnames), doc_id = stringr::str_c("doc_", doc_id)), tidyselect::starts_with("V"), names_to = "topic", values_to = "gamma")
     man/hellinger.Rd:
        topics2 = tidyr::pivot_longer(dplyr::mutate(tibble::as_tibble(rdirichlet(3, rep(5, 5)), rownames = "doc_id", .name_repair = tmfast:::make_colnames), doc_id = stringr::str_c("doc_", as.integer(doc_id) + 5)), tidyselect::starts_with("V"), names_to = "topic", values_to = "gamma")

### 16. Add `inst/CITATION`
- Given the arXiv preprint reference being added in item 6, also add `inst/CITATION` so `citation("tmfast")` returns the methods paper.
    [We'll use the JOSS paper; but that's after CRAN submission]


## Final submission checklist

<https://github.com/dhicks/tmfast/issues/2>
