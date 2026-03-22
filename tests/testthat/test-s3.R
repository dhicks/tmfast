## Minimal varimaxes fixture for testing S3 methods
set.seed(20260322)
mx = matrix(rnorm(100 * 20), nrow = 100, ncol = 20)
rownames(mx) = paste0('doc', 1:100)
colnames(mx) = paste0('term', 1:20)
fitted = varimax_irlba(mx, n = 5)
class(fitted) = c('tmfast', class(fitted))

test_that('check_dots_empty: loadings.varimaxes rejects unexpected args', {
    expect_error(loadings(fitted, k = 5, typo = TRUE), class = 'rlib_error_dots_nonempty')
})

test_that('check_dots_empty: scores.varimaxes rejects unexpected args', {
    expect_error(scores(fitted, k = 5, typo = TRUE), class = 'rlib_error_dots_nonempty')
})

test_that('check_dots_empty: rotation.varimaxes rejects unexpected args', {
    expect_error(rotation(fitted, k = 5, typo = TRUE), class = 'rlib_error_dots_nonempty')
})

test_that('check_dots_empty: tidy.tmfast rejects unexpected args', {
    expect_error(tidy(fitted, k = 5, typo = TRUE), class = 'rlib_error_dots_nonempty')
})

test_that('make_colnames', {
    expect_equal(make_colnames(letters[1:5]),
                 c('V1', 'V2', 'V3', 'V4', 'V5'))
    expect_equal(make_colnames(letters[1:15]),
                 c('V01', 'V02', 'V03', 'V04', 'V05',
                   'V06', 'V07', 'V08', 'V09', 'V10',
                   'V11', 'V12', 'V13', 'V14', 'V15'))
})
