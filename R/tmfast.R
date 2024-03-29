#' Fitting "topic models" with PCA+varimax

source('R/s3.R')

## TODO: tidiers that
## - convert loadings to dataframe
## - convert scores to dataframe
## - screeplot

#' Fit a varimax-rotated PCA using irlba
#' 
#' Extract `n` principal components from the matrix `mx` using `irlba`, then rotate the solution using `varimax`
#' @param mx Matrix of interest
#' @param n Number of principal components / varimax factors to return
#' @param prcomp_fn Function to use to extract principal components
#' @param prcomp_opts List of options to pass to `prcomp_fn`
#' @param varimax_fn Function to use for varimax rotation
#' @param varimax_opts List of options to pass to `varimax_fn`
#' @return A list of class `varimaxes`, with elements
#'   - `totalvar`: Total variance, from PCA
#'   - `sdev`:  Standard deviations of the extracted principal components
#'   - `loadings`: Varimax-rotated standardized loadings
#'   - `rotmat`:  Varimax rotation matrix
#'   - `scores`:  Varimax-rotated observation scores
#' @export 
varimax_irlba = function(mx, 
                         n,
                         prcomp_fn = irlba::prcomp_irlba,
                         prcomp_opts = NULL, 
                         varimax_fn = stats::varimax,
                         varimax_opts = NULL) {
    ## prcomp_irlba loses names
    rows = rownames(mx)
    cols = colnames(mx)
    ## PCA fit
    pca_fit = do.call(prcomp_fn, c(list(x = mx, 
                                        n = max(n)), 
                                   prcomp_opts))
    
    ## Varimax fit
    varimaxes = n |> 
        set_names() |> 
        map(fit_varimax, 
                    pca_fit, cols, rows, varimax_fn, varimax_opts)
    
    toreturn = list(totalvar = pca_fit$totalvar, 
                    sdev = pca_fit$sdev, 
                    n = n,
                    varimax = varimaxes)
    class(toreturn) = c('varimaxes', class(toreturn))
    return(toreturn)
}

#' Given a (rank n) PCA fit, return a rank k < n varimax fit
fit_varimax = function(k, pca, 
                       feature_names, obs_names, 
                       varimax_fn, varimax_opts) {
    raw_loadings = pca$rotation[,1:k] %*% diag(pca$sdev, k, k) |> 
        magrittr::set_rownames(feature_names)
    varimax_fit_prelim = do.call(varimax_fn, c(list(x = raw_loadings), 
                                               varimax_opts))
    
    ## Reverse factors with negative skew (left tails)
    varimax_fit = purrr::map(varimax_fit_prelim, 
                             ~ .x %*% diag(1 - 2*(psych::skew(varimax_fit_prelim$loadings) < 0)))
    assertthat::assert_that(all(psych::skew(varimax_fit$loadings) > 0), 
                            msg = 'Varimax loadings do not all have positive skew')
    
    ## Scores
    scores = scale(pca$x[,1:k]) %*% varimax_fit$rotmat |> 
        magrittr::set_rownames(obs_names)
    
    toreturn = list(loadings = varimax_fit$loadings, 
                    rotmat = varimax_fit$rotmat,
                    scores = scores)
    return(toreturn)
}

#' Fit a "topic model" using PCA+varimax
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

