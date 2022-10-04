#' Fitting "topic models" with PCA+varimax
#'
#'
NULL

#' Fit a varimax-rotated PCA using irlba
#'
#' Extract `n` principal components from the matrix `mx` using `irlba`, then rotate the solution using `varimax`
#' @param mx Matrix of interest
#' @param n Number of principal components / varimax factors to return; can take a vector of values
#' @param prcomp_fn Function to use to extract principal components
#' @param prcomp_opts List of options to pass to `prcomp_fn`
#' @param varimax_fn Function to use for varimax rotation
#' @param varimax_opts List of options to pass to `varimax_fn`
#' @return A list of class `varimaxes`, with elements
#'   - `totalvar`: Total variance, from PCA
#'   - `sdev`:  Standard deviations of the extracted principal components
#'   - `rotation`:  Rotation matrix (variable loadings) from PCA
#'   - `varimaxes`: A list of class `varimaxes`, containing one fitted varimax model for each value of `n`, with further elements
#'       - `loadings`: Varimax-rotated standardized loadings
#'       - `rotmat`:  Varimax rotation matrix
#'       - `scores`:  Varimax-rotated observation scores
#' @export
varimax_irlba = function(mx,
                         n,
                         prcomp_fn = irlba::prcomp_irlba,
                         prcomp_opts = NULL,
                         varimax_fn = stats::varimax,
                         varimax_opts = NULL,
                         retx = FALSE) {
    ## prcomp_irlba loses names
    rows = rownames(mx)
    cols = colnames(mx)
    ## PCA fit
    pca_fit = do.call(prcomp_fn, c(list(x = mx,
                                        n = max(n)),
                                   prcomp_opts))

    ## Varimax fit
    varimaxes = n |>
        purrr::set_names() |>
        purrr::map(fit_varimax,
                    pca_fit, cols, rows, varimax_fn, varimax_opts)

    toreturn = list(totalvar = pca_fit$totalvar,
                    sdev = pca_fit$sdev,
                    rows = rows,
                    cols = cols,
                    center = pca_fit$center,
                    scale = pca_fit$scale,
                    x = mx,
                    rotation = pca_fit$rotation,
                    n = n,
                    varimax = varimaxes)
    if (!retx) {
        toreturn$x = NULL
    }
    class(toreturn) = c('varimaxes', class(toreturn))
    return(toreturn)
}

#' Given a (rank `n`) PCA fit, return a rank `k < n` varimax fit
#'
#' @param k Desired rank of the fitted varimax model
#' @param pca Fitted PCA model
#' @param feature_names Names of the features (eg, data columns)
#' @param obs_names Names of the observations (eg, data rows)
#' @param varimax_fn Function to use for varimax rotation
#' @param varimax_opts Options passed to `varimax_fn`
#' @param positive_skew Should negative-skewed factors be flipped to have positive skew?
#' @param x Original data matrix; passed here if not included in `pca` (eg, via `retx = TRUE`)
#' @return List with components
#'     - `loadings`: Rotated feature loadings
#'     - `rotmat`:  Rotation matrix
#'     - `scores`:  Rotated observation scores
#' @details After the initial rotation, factors with negative skew (left tails) are flipped
#' @export
fit_varimax = function(k, pca,
                       feature_names, obs_names,
                       varimax_fn = stats::varimax,
                       varimax_opts = NULL,
                       positive_skew = TRUE,
                       x = NULL) {
    if (is.null(pca$x) && is.null(x)) {
        stop('Data matrix must be passed through either `pca` or `x`')
    }
    raw_loadings = pca$rotation[,1:k] %*% diag(pca$sdev, k, k) |>
        magrittr::set_rownames(feature_names)
    varimax_fit_prelim = do.call(varimax_fn, c(list(x = raw_loadings),
                                               varimax_opts))

    if (positive_skew) {
    ## Reverse factors with negative skew (left tails)
    varimax_fit = purrr::map(varimax_fit_prelim,
                             ~ .x %*% diag(1 - 2*(psych::skew(varimax_fit_prelim$loadings) < 0)))
    assertthat::assert_that(all(psych::skew(varimax_fit$loadings) > 0),
                            msg = 'Varimax loadings do not all have positive skew')
    } else {
        varimax_fit = varimax_fit_prelim
    }

    ## Scores
    if (is.null(x)) {
        scores = scale(pca$x[,1:k]) %*% varimax_fit$rotmat |>
            magrittr::set_rownames(obs_names)
    } else {
        scores = scale(x[,1:k]) %*% varimax_fit$rotmat |>
            magrittr::set_rownames(obs_names)
    }

    toreturn = list(loadings = varimax_fit$loadings,
                    rotmat = varimax_fit$rotmat,
                    scores = scores)
    return(toreturn)
}

#' Fit a topic model using PCA+varimax
#'
#' @param dtm Document-term matrix.  Either an object inheriting from `Matrix` or a long dataframe representation with row column `row`, column column `column`, and value column `n`.
#' @param n Number of topics to return
#' @param row In dataframe `dtm`, row column
#' @param column In dataframe `dtm`, column column
#' @param value In dataframe `dtm`, value column
#' @param ... Other arguments, passed to `varimax_irlba`
#' @return As per `varimax_irlba`, of class `tmfast`
#' @details If `dtm` is not a matrix, will be cast to a sparse matrix using `tidytext::case_sparse()`
#' @export
tmfast = function(dtm, n, row = doc, column = word, value = n, ...) {
    if (!inherits(dtm, 'Matrix')) {
        dtm = tidytext::cast_sparse(dtm, {{row}}, {{column}}, {{value}})
    }
    fitted = varimax_irlba(dtm, n, prcomp_opts = list(.scale = FALSE), ...)
    class(fitted) = c('tmfast', class(fitted))
    return(fitted)
}

#' Insert a topic model into a fitted `tmfast`
#'
#' Apply varimax rotation for a value of k less than the maximum already included in the tmfast.
#' @param fitted Fitted `tmfast` object
#' @param k Desired number of topics for new model
#' @param x Data matrix (document-term matrix), as Matrix object (eg, using `build_matrix()`)
#' @return `tmfast` object, as `fitted`, with additional topic model inserted
#' @export
insert_topics = function(fitted, k, x = NULL) {
    if (k %in% fitted$n) {
        return(fitted)
    }
    if (k > max(fitted$n)) {
        stop(glue::glue('Can only insert topic models with at most {max(fitted$n)} topics'))
    }

    if (length(k) > 1) {
        stop('Currently can only insert 1 topic model at a time')
    }

    if (is.null(fitted$x) && is.null(x)) {
        stop('Data matrix missing; pass using x')
    }

    if (is.null(x)) {
        new_varimax = fit_varimax(k, fitted,
                                  feature_names = fitted$cols,
                                  obs_names = fitted$rows)
    } else {
        new_varimax = fit_varimax(k, fitted,
                                  feature_names = fitted$cols,
                                  obs_names = fitted$rows,
                                  x = x)
    }
    fitted_tmf$n = c(fitted_tmf$n, k)
    fitted_tmf$varimax[[as.character(k)]] = new_varimax
    return(fitted_tmf)
}
