library(tmfast)

set.seed(2022-06-09)
topics1 = rdirichlet(3, rep(5, 5)) |>
    tibble::as_tibble(rownames = 'doc_id', .name_repair = make_colnames) |>
    dplyr::mutate(doc_id = stringr::str_c('doc_', doc_id)) |>
    tidyr::pivot_longer(tidyselect::starts_with('V'),
                        names_to = 'topic',
                        values_to = 'gamma')
topics2 = rdirichlet(3, rep(5, 5)) |>
    tibble::as_tibble(rownames = 'doc_id', .name_repair = make_colnames) |>
    dplyr::mutate(doc_id = stringr::str_c('doc_', as.integer(doc_id) + 5)) |>
    tidyr::pivot_longer(tidyselect::starts_with('V'),
                        names_to = 'topic',
                        values_to = 'gamma')
hellinger(topics1, doc_id, prob1 = 'gamma', df = TRUE)
hellinger(topics1, doc_id, prob1 = 'gamma',
          topicsdf2 = topics2, id2 = doc_id, prob2 = 'gamma')
