#' Expected entropy for samples from a Dirichlet distribution
#'
#' Samples P = <p1, p2, ..., pk> from Dirichlet distribution with parameter alpha = <alpha1, alpha2, ..., alphak> can be treated as categorical probability distributions with entropy H(P) = sum(-p log(p)).  This function calculates the expected entropy E[H(P)] given alpha.
#'
#' After <https://math.stackexchange.com/questions/2266285/expected-entropy-based-on-dirichlet-distribution/3195376#3195376>
#' @param alpha Dirichlet parameter
#' @param k If length(alpha) is 1, number of components in symmetric Dirichlet distribution
#' @return Expected entropy E[H(P)] in bits (log2 scale)
#' @examples
#' alpha = peak_alpha(50, 1)
#' set.seed(1357)
#' rdirichlet(500, alpha) |>
#'   apply(1, \(p)(sum(-p*log2(p), na.rm = TRUE))) |>
#'   mean()
#' expected_entropy(alpha)
expected_entropy = function(alpha, k = NULL) {
    if (identical(length(alpha), 1L)) {
        assertthat::assert_that(!is.null(k),
                                msg = 'When alpha is length 1, k must not be null')
        alpha = rep(alpha, k)
    }
    alpha0 = sum(alpha)
    sum(alpha / alpha0 * (digamma(alpha0 + 1) - digamma(alpha + 1))) * log2(exp(1))
}


#' Solve the equation to find the desired exponent
#'
#' After <https://stats.stackexchange.com/questions/521582/controlling-the-entropy-of-a-distribution>
#' @param p Initial distribution
#' @param target_H Desired entropy for the transformed distribution
#' @param interval Range of exponents within which to search
#' @param return_full Return the full uniroot() output?
solve_power = function(p,
                       target_H,
                       interval = c(0.1, 100),
                       return_full = FALSE) {
    ## Entropy of the distribution after transformation
    transformed_entropy = function(p) {
        function(beta) {
            Z = sum(p^beta, na.rm = TRUE)
            - 1/Z * sum(p^beta * beta * log2(p), na.rm = TRUE) + log2(Z)
        }
    }

    soln = uniroot(\(beta) transformed_entropy(p)(beta) - target_H,
            interval)
    if (return_full) {
        return(soln)
    } else {
        return(soln$root)
    }
}
# doc1 = beta$beta[1:500]
# solve_power(doc1, target_entropy)

#' Renormalize tidied distributions
#'
#' Given a tidied dataframe of topic-doc or word-topic distributions and a target entropy, first finds the exponent needed to adjust the temperature of each distribution separately to (approximately) match the target entropy (`solve_power()`).  Applies the median such exponent to every distribution.
#' @param tidy_df The tidied distribution dataframe
#' @param group_col Grouping column, RHS of the conditional probability distribution, eg, topics for word-topic distributions
#' @param p_col Column containing the probability for each category (eg, word) conditional on the group (eg, topic)
#' @param target_entropy Target entropy
#' @param keep_original Keep original probabilities?
#' @returns A dataframe with an added column of the form `p_col_rn` containing the renormalized probabilities (if `keep_original` is `TRUE`) or renormalized values in `p_col` (if `keep_original` is `FALSE`) and an `exponent` attribute containing the exponent used for renormalization.
renorm = function(tidy_df,
                  group_col,
                  p_col,
                  target_entropy,
                  keep_original = FALSE) {
    ## 1. Get the exponent for each distribution
    ## 2. Get the median exponent
    exponent = tidy_df |>
        dplyr::group_by({{ group_col }}) |>
        dplyr::summarize(exponent = solve_power({{ p_col }},
                                         target_entropy)) |>
        dplyr::pull(exponent) |>
        median()

    ## 3. Apply to all distributions
    if (keep_original) {
        newcol = rlang::englue('{{ p_col }}_rn')
    } else {
        newcol = rlang::englue('{{ p_col }}')
    }
    tidy_df |>
        dplyr::group_by({{ group_col }}) |>
        dplyr::mutate({{ newcol }} := {{ p_col }}^exponent / sum({{ p_col }}^exponent)) |>
        dplyr::ungroup() |>
        magrittr::set_attr('exponent', exponent)
}
# renorm(beta, topic, beta, target_entropy)


## Development example code
# library(tmfast)
# library(tidyverse)
# # 10 word-topic distributions, 500-word vocabulary
# beta = rdirichlet(50, .05, k = 2000) |>
#     tibble:::as.tibble() |>
#     dplyr::mutate(topic = dplyr::row_number()) |>
#     tidyr::pivot_longer(-topic,
#                         names_to = 'word',
#                         values_to = 'beta')
# # "raw" entropy is ~5.5
# beta |>
#     group_by(topic) |>
#     mutate(H_term = -beta*log2(beta)) |>
#     summarize(H = sum(H_term))
#
# # target entropy is about 3
# target_entropy = tmfast:::expected_entropy(.01, n_distinct(beta$word))
#
# renorm(beta, topic, beta, target_entropy, keep_original = TRUE) |>
#     group_by(topic) |>
#     summarize(H = sum(-beta * log2(beta)),
#               H_rn = sum(-beta_rn * log2(beta_rn)))
#     # ggplot(aes(x = word)) +
#     # geom_linerange(aes(ymin = beta, ymax = beta_rn)) +
#     # geom_point(aes(y = beta_rn), size = .2) +
#     # facet_wrap(vars(topic))
