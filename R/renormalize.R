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
#'   apply(1, entropy) |>
#'   mean()
#' expected_entropy(alpha)
#' @export
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
#' @return Numeric value of the desired exponent
#' @export
solve_power = function(p,
                       target_H,
                       return_full = FALSE) {
    ## Entropy of the distribution after transformation
    transformed_entropy = function(p) {
        function(beta) {
            Z = sum(p^beta, na.rm = TRUE)
            - 1/Z *
                sum(p^beta * beta * log2(p), na.rm = TRUE) +
                log2(Z)
        }
    }

    soln = purrr::possibly(uniroot, otherwise = NA_real_)(
        \(beta) {transformed_entropy(p)(beta) - target_H},
        interval = c(.1, 10),
        extendInt = 'yes',
        maxiter = 3000)
    if ((return_full || identical(soln, NA_real_)) || is.na(soln$root)) {
        return(soln)
    } else {
        return(soln$root)
    }
}
# doc1 = beta$beta[1:500]
# solve_power(doc1, target_entropy)

#' Find target power for renormalization
#'
#' Given a tidied dataframe of topic-doc or word-topic distributions and a target entropy, find the mean exponent needed to adjust the temperature of each distribution to approximately match the target entropy.
#' @param tidy_df The tidied distribution dataframe
#' @param Grouping column, RHS of the conditional probability distribution, eg, topics for word-topic distributions
#' @param p_col Column containing the probability for each category (eg, word) conditional on the group (eg, topic)
#' @param target_entropy Target entropy
#' @returns Mean exponent to renormalize to the target entropy
#' @export
target_power = function(tidy_df,
                        group_col,
                        p_col,
                        target_entropy) {
    powers = tidy_df |>
        dplyr::group_by({{ group_col }}) |>
        dplyr::summarize(H = entropy({{ p_col }}),
                  power = solve_power({{ p_col }}, target_entropy)) |>
        dplyr::pull(power)
    if (sum(is.na(powers)) > 0.1 * length(powers)) {
        warning('More than 10% of powers could not be calculated')
    }
    median(powers, na.rm = TRUE)
}

#' Renormalize tidied distributions
#'
#' Given a tidied dataframe of topic-doc or word-topic distributions and a exponent, renormalizes the distributions.
#' @param tidy_df The tidied distribution dataframe
#' @param group_col Grouping column, RHS of the conditional probability distribution, eg, topics for word-topic distributions
#' @param p_col Column containing the probability for each category (eg, word) conditional on the group (eg, topic)
#' @param exponent Exponent to use in renormalization
#' @param keep_original Keep original probabilities?
#' @returns A dataframe with (if `keep_original` is `TRUE`) an added column of the form `p_col_rn` containing the renormalized probabilities or (if `keep_original` is `FALSE`) renormalized values in `p_col`.
#' @export
renorm = function(tidy_df,
                  group_col,
                  p_col,
                  exponent,
                  keep_original = FALSE) {
    if (keep_original) {
        newcol = rlang::englue('{{ p_col }}_rn')
    } else {
        newcol = rlang::englue('{{ p_col }}')
    }
    tidy_df |>
        dplyr::group_by({{ group_col }}) |>
        dplyr::mutate({{ newcol }} := {{ p_col }}^exponent / sum({{ p_col }}^exponent)) |>
        dplyr::ungroup()
}
# renorm(beta, topic, beta, target_entropy)

