## Scratchwork for S3 class
## TODO: 
## - validator: 
##      - length(sdev) == max(n)
##      - names(varimax) == as.character(n)
##      - each varimax sol'n has ncol corresponding to name
## - tidy method
## - documentation

loadings = function(x, ...) {
    UseMethod("loadings")
}
loadings.default = function(x) {
    # message('loadings.default')
    stats::loadings(x)
}
loadings.varimaxtm = function(x, k) {
    # message('loadings.varimaxtm')
    assertthat::assert_that(k %in% x$n, 
                            msg = glue::glue('Rank {k} not in fitted model'))
    loadings(x$varimax[[as.character(k)]])
}
# loadings(fitted, 5)
# loadings(fitted, 6)

scores = function(x, ...) {
    UseMethod("scores")
}
scores.varimaxtm = function(x, k) {
    assertthat::assert_that(k %in% x$n, 
                            msg = glue::glue('Rank {k} not in fitted model'))
    x$varimax[[as.character(k)]]$scores
}
# scores(fitted, 5)
# scores(fitted, 6)

rotation = function(x, ...) {
    UseMethod('rotation')
}
rotation.varimaxtm = function(x, k) {
    assertthat::assert_that(k %in% x$n, 
                            msg = glue::glue('Rank {k} not in fitted model'))
    x$varimax[[as.character(k)]]$rotmat
}
# rotation(fitted, 5)

tidy = function(x, ...) {
    UseMethod('tidy')
}
tidy.varimaxtm = function(x, k, 
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
            loadings_mx = rotation %*% loadings_mx
        }
        dataf = loadings_mx |> 
            as_tibble(rownames = 'token') |> 
            pivot_longer(starts_with('V'), 
                         names_to = 'topic', 
                         values_to = 'beta') |> 
            ## Trim at 0, then normalize to sum to 1
            group_by(topic) |> 
            filter(beta > 0) |>
            mutate(beta = beta / sum(beta)) |> 
            ungroup()
        return(dataf)
    }
    if (identical(matrix, 'gamma')) {
        if (!df) {
            warning('Varimax document scores, not rotated, nudged, or normalized')
            return(scores(x, k))
        }
        scores_mx = scores(x, k)
        if (!is.null(rotation)) {
            message('rotating')
            scores_mx = scores_mx %*% t(rotation)
        }
        dataf = scores_mx |> 
            as_tibble(rownames = 'doc') |> 
            pivot_longer(starts_with('V'), 
                         names_to = 'topic', 
                         values_to = 'gamma') |> 
            ## Nudge everything so the minimum value is 0, then normalize
            group_by(doc) |> 
            mutate(gamma = gamma - min(gamma)) |> 
            mutate(gamma = gamma / sum(gamma)) |> 
            ungroup()
        return(dataf)
    }
}
# tidy(fitted, 5, 'gamma', df = TRUE)
