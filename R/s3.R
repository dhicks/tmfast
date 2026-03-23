#' Extract a PCA/varimax loadings matrix
#'
#' @param x Object to dispatch on
#' @param ... Passed to methods
#' @export
loadings = function(x, ...) {
      UseMethod("loadings")
}
#' @export
loadings.default = function(x, ...) {
      rlang::check_dots_empty()
      # message('loadings.default')
      stats::loadings(x)
}
#' @export
loadings.varimaxes = function(x, k, ...) {
      rlang::check_dots_empty()
      # message('loadings.varimaxtm')
      assertthat::assert_that(
            k %in% x$n,
            msg = glue::glue('Rank {k} not in fitted model')
      )
      loadings(x$varimax[[as.character(k)]])
}
# loadings(fitted, 5)
# loadings(fitted, 6)

#' Extract item scores from a fitted PCA/varimax model
#'
#' @param x Object to dispatch on
#' @param ... Passed to methods
#' @export
scores = function(x, ...) {
      UseMethod("scores")
}
#' @export
scores.varimaxes = function(x, k, ...) {
      rlang::check_dots_empty()
      assertthat::assert_that(
            k %in% x$n,
            msg = glue::glue('Rank {k} not in fitted model')
      )
      x$varimax[[as.character(k)]]$scores
}
# scores(fitted, 5)
# scores(fitted, 6)

#' Extract varimax rotation
#'
#' @param x Object to dispatch on
#' @param ... Passed to methods
#' @export
rotation = function(x, ...) {
      UseMethod('rotation')
}
#' @export
rotation.varimaxes = function(x, k, ...) {
      rlang::check_dots_empty()
      assertthat::assert_that(
            k %in% x$n,
            msg = glue::glue('Rank {k} not in fitted model')
      )
      x$varimax[[as.character(k)]]$rotmat
}
# rotation(fitted, 5)

#' Project new data into PCA score space
#'
#' @param object Fitted `varimaxes` or `tmfast` object
#' @param newdata Document-term matrix (observations x terms) to project
#' @details Projects `newdata` through the PCA rotation stored in `object`, returning
#'   raw PCA scores (not varimax scores). Intended for use in pipelines that combine
#'   new data with an existing fitted model (e.g., `insert_topics()`). Fragile: `newdata`
#'   must share the vocabulary of the training DTM, and the centering/scaling stored in
#'   `object` must match how the training data was prepared.
#'
#'   **Memory warning**: `scale()` coerces sparse matrices to dense. For large DTMs,
#'   this can be a substantial memory hazard. This mirrors the behavior of `prcomp_irlba`
#'   itself, which is why PCA scores are computed once at fit time and not re-projected
#'   on demand.
#' @param ... Not used; included for S3 method compatibility.
#' @return Matrix of PCA scores (n_obs x max_k)
#' @importFrom stats predict
#' @export
predict.varimaxes = function(object, newdata, ...) {
    rlang::check_dots_empty()
    nm = object$cols
    if (!is.null(nm)) {
        if (!all(nm %in% colnames(newdata)))
            stop("'newdata' does not have named columns matching the original vocabulary")
        newdata = newdata[, nm, drop = FALSE]
    } else {
        if (ncol(newdata) != nrow(object$rotation))
            stop("'newdata' does not have the correct number of columns")
    }
    scale(newdata, center = object$center, scale = object$scale) %*% object$rotation
}

#' Make colnames
#'
#' Helper function to make matrix column names of the form 'V09'
#' @noRd
make_colnames = function(names, prefix = 'V') {
      n = length(names)
      1:n |>
            as.character() |>
            stringr::str_pad(stringr::str_length(n), pad = '0') %>%
            stringr::str_c(prefix, .)
}


#' @importFrom generics tidy
#' @export
generics::tidy

#' Extract beta and gamma matrices from `tmfast` objects
#'
#' @param x `tmfast` object
#' @param k Index (number of topics/factors)
#' @param matrix Desired matrix, either word-topic (`beta`) or topic-doc distributions (`gamma`)
#' @param df Return a long dataframe (default) or wide matrix?
#' @param exponent Renormalize the probabilities using a given exponent  Applies only for `df == TRUE`
#' @param keep_original If renormalizing, return original (pre-renormalized) probabilities?
#' @param rotation Optional rotation matrix; see details
#' @param ... Not used; required for S3 method compatibility
#' @return A long dataframe, with one row per word-topic or topic-doc combination. Column names depend on the value of `matrix`.
#' @details If `rotation` is not `NULL`, loadings/scores will be rotated.  This might be used to align the fitted topics with known true topics, as in the `journal_specific` simulation.  Loadings are left-multiplied by the given rotation, while scores are right-multiplied by the transpose of the given rotation.
#' @export
tidy.tmfast = function(
      x,
      k,
      matrix = 'beta',
      df = TRUE,
      exponent = NULL,
      keep_original = FALSE,
      rotation = NULL,
      ...
) {
      rlang::check_dots_empty()
      assertthat::assert_that(
            k %in% x$n,
            msg = glue::glue('Rank {k} not in fitted model')
      )
      assertthat::assert_that(
            matrix %in% c('beta', 'gamma'),
            msg = glue::glue('Matrix argument {matrix} invalid')
      )
      if (identical(matrix, 'beta')) {
            if (!df) {
                  cli::cli_alert_warning(
                        'Varimax token loadings, not rotated, trimmed, or normalized'
                  )
                  return(loadings(x, k))
            }
            loadings_mx = loadings(x, k)
            if (!is.null(rotation)) {
                  cli::cli_alert_info('Rotating loadings')
                  loadings_mx = rotation %*% loadings_mx
            }
            dataf = loadings_mx |>
                  tibble::as_tibble(
                        rownames = 'token',
                        .name_repair = make_colnames
                  ) |>
                  tidyr::pivot_longer(
                        starts_with('V'),
                        names_to = 'topic',
                        values_to = 'beta'
                  ) |>
                  ## Trim at 0, then normalize to sum to 1
                  dplyr::group_by(topic) |>
                  dplyr::filter(beta > 0) |>
                  dplyr::mutate(beta = beta / sum(beta)) |>
                  dplyr::ungroup()
            if (!is.null(exponent)) {
                  dataf = renorm(dataf, topic, beta, exponent, keep_original)
            }
            return(dataf)
      }
      if (identical(matrix, 'gamma')) {
            if (!df) {
                  cli::cli_alert_warning(
                        'Varimax document scores, not rotated, nudged, or normalized'
                  )
                  return(scores(x, k))
            }
            scores_mx = scores(x, k)
            if (!is.null(rotation)) {
                  cli::cli_alert_info('Rotating scores')
                  scores_mx = scores_mx %*% t(rotation)
            }
            dataf = scores_mx |>
                  tibble::as_tibble(
                        rownames = 'document',
                        .name_repair = make_colnames
                  ) |>
                  tidyr::pivot_longer(
                        starts_with('V'),
                        names_to = 'topic',
                        values_to = 'gamma'
                  ) |>
                  ## Nudge everything so the minimum value is 0, then normalize
                  dplyr::group_by(document) |>
                  dplyr::mutate(gamma = gamma - min(gamma)) |>
                  dplyr::mutate(gamma = gamma / sum(gamma)) |>
                  dplyr::ungroup()
            if (!is.null(exponent)) {
                  dataf = renorm(
                        dataf,
                        document,
                        gamma,
                        exponent,
                        keep_original
                  )
            }
            return(dataf)
      }
}
# tidy(fitted, 5, 'gamma', df = TRUE)

#' Extract gamma or beta matrices for all topics
#'
#' @param x `tmfast` object
#' @param matrix Desired matrix, `'beta'` or `'gamma'`
#' @param ... Other arguments, passed to `tidy.tmfast()`
#' @return A long dataframe, with one row per word-topic or topic-doc combination. Column names depend on the value of `matrix`.
#' @export
tidy_all = function(x, matrix = 'beta', ...) {
      k = x$n |>
            rlang::set_names()
      purrr::map_dfr(k, ~ tidy(x, .x, matrix = matrix, ...), .id = 'k') |>
            dplyr::mutate(k = as.integer(k)) |>
            dplyr::select(k, tidyselect::everything())
}
