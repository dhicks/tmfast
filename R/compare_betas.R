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
#' @examples
#' set.seed(42)
#' vocab = letters[1:5]
#' make_beta = function(k) {
#'   rdirichlet(k, rep(1, length(vocab))) |>
#'     tibble::as_tibble(.name_repair = ~vocab) |>
#'     dplyr::mutate(topic = paste0('t', dplyr::row_number())) |>
#'     tidyr::pivot_longer(-topic, names_to = 'token', values_to = 'beta')
#' }
#' beta1 = make_beta(3)
#' beta2 = make_beta(4)
#' compare_betas(beta1, vocab = vocab)
#' compare_betas(beta1, beta2, vocab = vocab)
#' @export
compare_betas = function(beta1, beta2 = NULL, vocab) {
      fill = function(beta) {
            beta |>
                  tidyr::complete(
                        "token" = vocab,
                        .data$topic,
                        fill = list(beta = 0)
                  ) |>
                  build_matrix("topic", "token", "beta") |>
                  (\(x) x[, vocab])()
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
