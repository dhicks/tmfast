test_that('varimax_irlba: output has correct class and structure', {
    set.seed(20260324)
    mx = matrix(rnorm(50 * 10), nrow = 50)
    rownames(mx) = paste0('doc', 1:50)
    colnames(mx) = paste0('term', 1:10)
    result = varimax_irlba(mx, n = 3)
    expect_s3_class(result, 'varimaxes')
    expect_equal(result$n, 3)
    expect_named(result$varimax, '3')
    expect_equal(result$rows, rownames(mx))
    expect_equal(result$cols, colnames(mx))
    expect_null(result$x)   # retx = FALSE by default
})

test_that('varimax_irlba: retx = TRUE stores data matrix', {
    set.seed(20260324)
    mx = matrix(rnorm(50 * 10), nrow = 50)
    result = varimax_irlba(mx, n = 3, retx = TRUE)
    expect_equal(result$x, mx)
})

test_that('varimax_irlba: multiple n values produce one fit per k', {
    set.seed(20260324)
    mx = matrix(rnorm(50 * 10), nrow = 50)
    result = varimax_irlba(mx, n = c(2, 3, 4))
    expect_equal(sort(result$n), c(2, 3, 4))
    expect_named(result$varimax, c('2', '3', '4'))
})

test_that('varimax_irlba: loadings and scores have correct dimensions', {
    set.seed(20260324)
    mx = matrix(rnorm(50 * 10), nrow = 50)
    rownames(mx) = paste0('doc', 1:50)
    colnames(mx) = paste0('term', 1:10)
    result = varimax_irlba(mx, n = 3)
    expect_equal(dim(loadings(result, k = 3)), c(10, 3))
    expect_equal(dim(scores(result, k = 3)), c(50, 3))
})

test_that('fit_varimax', {
    pr_fit = prcomp(swiss, rank. = 2)
    varmax1 = pr_fit$rotation %*% diag(pr_fit$sdev, 2, 2) |>
        varimax()
    varmax2 = fit_varimax(2, pr_fit,
                          feature_names = colnames(swiss),
                          obs_names = rownames(swiss),
                          positive_skew = FALSE)
    expect_equal(varmax1$rotmat, varmax2$rotmat)
    expect_equal(varmax1$loadings, varmax2$loadings)

    varmax3 = fit_varimax(2, pr_fit,
                          feature_names = colnames(swiss),
                          obs_names = rownames(swiss))
    expect_true(all(psych::skew(varmax3$loadings) > 0))
})
