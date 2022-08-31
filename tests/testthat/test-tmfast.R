test_that('varimax_irlba', {
  warning('Rely on tests in `irlba`')
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
