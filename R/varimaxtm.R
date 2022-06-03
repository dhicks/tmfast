#' Fitting "topic models" with PCA+varimax

## TODO: tidiers that
## - convert loadings to dataframe
## - convert scores to dataframe
## - screeplot
## TODO: accept multiple values of n, w/ single PCA fit but multiple varimax fits

#' Fit a varimax-rotated PCA using irlba
#' 
#' Extract `n` principal components from the matrix `mx` using `irlba`, then rotate the solution using `varimax`
#' @param mx Matrix of interest
#' @param n Number of principal components / varimax factors to return
#' @param prcomp_fn Function to use to extract principal components
#' @param prcomp_opts List of options to pass to `prcomp_fn`
#' @param varimax_fn Function to use for varimax rotation
#' @param varimax_opts List of options to pass to `varimax_fn`
#' @return A list with elements
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
    pca_fit = do.call(prcomp_fn, c(list(x = mx, n = n), 
                                   prcomp_opts))
    
    ## Varimax fit
    raw_loadings = pca_fit$rotation[,1:n] %*% diag(pca_fit$sdev, n, n) |> 
        magrittr::set_rownames(cols)
    varimax_fit_prelim = do.call(varimax_fn, c(list(x = raw_loadings), 
                                               varimax_opts))
    
    ## Reverse factors with negative skew (left tails)
    varimax_fit = purrr::map(varimax_fit_prelim, 
                             ~ .x %*% diag(1 - 2*(psych::skew(varimax_fit_prelim$loadings) < 0)))
    assertthat::assert_that(all(psych::skew(varimax_fit$loadings) > 0), 
                            msg = 'Varimax loadings do not all have positive skew')
    
    ## Scores
    scores = scale(pca_fit$x[,1:n]) %*% varimax_fit$rotmat |> 
        magrittr::set_rownames(rows)
    
    toreturn = list(totalvar = pca_fit$totalvar, 
                    sdev = pca_fit$sdev, 
                    loadings = varimax_fit$loadings, 
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
#' @return As per `varimax_irlba`
#' @details If `dtm` is not a matrix, will be cast to a sparse matrix using `tidytext::case_sparse()`
#' @export
varimax_tm = function(dtm, n, row = doc, column = word, value = n, ...) {
    if (!inherits(dtm, 'Matrix')) {
        dtm = tidytext::cast_sparse(dtm, {{row}}, {{column}}, {{value}})
    }
    return(varimax_irlba(dtm, n, prcomp_opts = list(.scale = FALSE), ...))
}
