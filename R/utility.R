#' Entropy of a distribution
#'
#' @param p Discrete probability distribution
#' @param base Desired base for entropy, eg, 2 for bits
#' @returns Calculated Shannon entropy
#' @examples
#' entropy(c(0.5, 0.5))
#' entropy(c(0.9, 0.1))
#' @export
entropy = function(p, base = 2) {
    assertthat::assert_that(assertthat::are_equal(sum(p),
                                                  1))
    sum(-p * log(p, base = base), na.rm = TRUE)
}
