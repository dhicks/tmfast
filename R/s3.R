## Scratchwork for S3 class
## TODO:
## - validator:
##      - length(sdev) == max(n)
##      - names(varimax) == as.character(n)
##      - each varimax sol'n has ncol corresponding to name
## - tidy method
## - documentation

#' Extract a PCA/varimax loadings matrix
#'
#' @export
loadings = function(x, ...) {
    UseMethod("loadings")
}
#' @export
loadings.default = function(x) {
    # message('loadings.default')
    stats::loadings(x)
}
#' @export
loadings.varimaxes = function(x, k) {
    # message('loadings.varimaxtm')
    assertthat::assert_that(k %in% x$n,
                            msg = glue::glue('Rank {k} not in fitted model'))
    loadings(x$varimax[[as.character(k)]])
}
# loadings(fitted, 5)
# loadings(fitted, 6)

#' Extract item scores from a fitted PCA/varimax model
#'
#' @export
scores = function(x, ...) {
    UseMethod("scores")
}
#' @export
scores.varimaxes = function(x, k) {
    assertthat::assert_that(k %in% x$n,
                            msg = glue::glue('Rank {k} not in fitted model'))
    x$varimax[[as.character(k)]]$scores
}
# scores(fitted, 5)
# scores(fitted, 6)

#' Extract varimax rotation
#'
#' @export
rotation = function(x, ...) {
    UseMethod('rotation')
}
#' @export
rotation.varimaxes = function(x, k) {
    assertthat::assert_that(k %in% x$n,
                            msg = glue::glue('Rank {k} not in fitted model'))
    x$varimax[[as.character(k)]]$rotmat
}
# rotation(fitted, 5)

#' @export
tidy = function(x, ...) {
    UseMethod('tidy')
}
#' Extract beta and gamma matrices from `tmfast` objects
#'
#' @param x `tmfast` object
#' @param k Index (number of topics/factors)
#' @param matrix Desired matrix, either word-topic (`beta`) or topic-doc distributions (`gamma`)
#' @param df Return a long dataframe (default) or wide matrix?
#' @param rotation Optional rotation matrix; see details
#' @return A long dataframe, with one row per word-topic or topic-doc combination. Column names depend on the value of `matrix`.
#' @details If `rotation` is not `NULL`, loadings/scores will be rotated.  This might be used to align the fitted topics with known true topics, as in the `journal_specific` simulation.  Loadings are left-multiplied by the given rotation, while scores are right-multiplied by the transpose of the given rotation.
#' @export
tidy.tmfast = function(x, k,
                          matrix = c('beta', 'gamma'),
                          df = TRUE,
                          rotation = NULL) {
    assertthat::assert_that(k %in% x$n,
                            msg = glue::glue('Rank {k} not in fitted model'))
    assertthat::assert_that(matrix %in% c('beta', 'gamma'),
                            msg = glue::glue('Matrix argument {matrix} invalid'))
    if (identical(matrix, 'beta')) {
        if (!df) {
            warning('Varimax token loadings, not rotated, trimmed, or normalized')
            return(loadings(x, k))
        }
        loadings_mx = loadings(x, k)
        if (!is.null(rotation)) {
            warning('Rotating loadings')
            loadings_mx = rotation %*% loadings_mx
        }
        dataf = loadings_mx |>
            tibble::as_tibble(rownames = 'token') |>
            tidyr::pivot_longer(starts_with('V'),
                         names_to = 'topic',
                         values_to = 'beta') |>
            ## Trim at 0, then normalize to sum to 1
            dplyr::group_by(topic) |>
            dplyr::filter(beta > 0) |>
            dplyr::mutate(beta = beta / sum(beta)) |>
            dplyr::ungroup()
        return(dataf)
    }
    if (identical(matrix, 'gamma')) {
        if (!df) {
            warning('Varimax document scores, not rotated, nudged, or normalized')
            return(scores(x, k))
        }
        scores_mx = scores(x, k)
        if (!is.null(rotation)) {
            warning('Rotating scores')
            scores_mx = scores_mx %*% t(rotation)
        }
        dataf = scores_mx |>
            tibble::as_tibble(rownames = 'doc') |>
            tidyr::pivot_longer(starts_with('V'),
                         names_to = 'topic',
                         values_to = 'gamma') |>
            ## Nudge everything so the minimum value is 0, then normalize
            dplyr::group_by(doc) |>
            dplyr::mutate(gamma = gamma - min(gamma)) |>
            dplyr::mutate(gamma = gamma / sum(gamma)) |>
            dplyr::ungroup()
        return(dataf)
    }
}
# tidy(fitted, 5, 'gamma', df = TRUE)
