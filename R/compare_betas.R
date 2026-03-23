#' Compare topic-word distributions using Hellinger distance
#'
#' Computes pairwise Hellinger distances between topics from one or two fitted
#' models. Tokens missing from a beta dataframe are filled with probability 0
#' before comparison, so both models need not share the same vocabulary.
#'
#' @param beta1 Tidy beta dataframe with columns `token`, `topic`, and `beta`,
#'   as returned by `tidy(model, matrix = 'beta')`.
#' @param beta2 Optional second tidy beta dataframe in the same format. If
#'   `NULL` (default), pairwise distances among the topics in `beta1` are
#'   returned.
#' @param vocab Character vector of vocabulary tokens used to align the column
#'   space of both matrices. Tokens in `beta1` or `beta2` that are not in
#'   `vocab` are dropped; tokens in `vocab` absent from a beta are filled with
#'   probability 0.
#' @return Numeric matrix of Hellinger distances. Dimensions are k1 × k1 when
#'   `beta2 = NULL`, or k1 × k2 when two beta dataframes are supplied, where
#'   k1 and k2 are the number of topics in each model.
compare_betas = function(beta1, beta2 = NULL, vocab) {
      fill = function(beta) {
            beta |>
                  tidyr::complete(
                        token = vocab,
                        topic,
                        fill = list(beta = 0)
                  ) |>
                  build_matrix(topic, token, beta) %>%
                  .[, vocab]
      }
      beta1 = fill(beta1)
      if (is.null(beta2)) {
            beta2 = beta1
      } else {
            beta2 = fill(beta2)
      }

      assertthat::assert_that(all(colnames(beta1) == colnames(beta2)))

      hellinger(beta1, beta2)
}
