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

test_that('softmax: sums to 1, all positive, handles negatives', {
    x = c(-2, -1, 0, 1, 2)
    result = softmax(x)
    expect_equal(sum(result), 1)
    expect_true(all(result > 0))
})

test_that('softmax: numerically stable on large values', {
    x = c(1000, 1001, 1002)
    result = softmax(x)
    expect_equal(sum(result), 1)
    expect_false(any(is.nan(result)))
})

test_that('tidy beta: all tokens present, sums to 1 per topic, all positive', {
    beta = tidy(fitted, k = 5, matrix = 'beta')
    ## all 20 terms appear in each topic
    expect_equal(nrow(beta), 20 * 5)
    ## sums to 1 within each topic
    topic_sums = beta |>
        dplyr::group_by(topic) |>
        dplyr::summarize(s = sum(beta))
    expect_equal(topic_sums$s, rep(1, 5), tolerance = 1e-10)
    expect_true(all(beta$beta > 0))
})

test_that('tidy gamma: all topics present, sums to 1 per document, all positive', {
    gamma = tidy(fitted, k = 5, matrix = 'gamma')
    ## all 5 topics appear for each of 100 documents
    expect_equal(nrow(gamma), 100 * 5)
    ## sums to 1 within each document
    doc_sums = gamma |>
        dplyr::group_by(document) |>
        dplyr::summarize(s = sum(gamma))
    expect_equal(doc_sums$s, rep(1, 100), tolerance = 1e-10)
    expect_true(all(gamma$gamma > 0))
})
