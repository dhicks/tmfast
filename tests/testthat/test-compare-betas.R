## Fixtures: minimal tidy beta tibbles with known math
vocab = c('a', 'b', 'c', 'd')

## T1 uses only a,b; T2 uses only c,d — disjoint distributions
beta_disjoint = tibble::tibble(
    token = c('a', 'b', 'c', 'd'),
    topic = c('T1', 'T1', 'T2', 'T2'),
    beta  = c(0.5, 0.5, 0.5, 0.5)
)

## Both topics identical: uniform over a,b
beta_identical = tibble::tibble(
    token = c('a', 'b', 'a', 'b'),
    topic = c('T1', 'T1', 'T2', 'T2'),
    beta  = c(0.5, 0.5, 0.5, 0.5)
)

## Three topics over the same vocab (for cross-model test)
beta_3topics = dplyr::bind_rows(
    beta_disjoint,
    tibble::tibble(token = c('a', 'b', 'c', 'd'), topic = 'T3', beta = 0.25)
)

## ---- compare_betas() --------------------------------------------------------

test_that('compare_betas: self-comparison returns square matrix', {
    result = compare_betas(beta_disjoint, vocab = vocab)
    expect_equal(dim(result), c(2L, 2L))
})

test_that('compare_betas: self-comparison diagonal is 0', {
    result = compare_betas(beta_disjoint, vocab = vocab)
    expect_equal(diag(as.matrix(result)), c(0, 0), ignore_attr = TRUE)
})

test_that('compare_betas: disjoint topics have Hellinger distance 1', {
    result = compare_betas(beta_disjoint, vocab = vocab)
    expect_equal(result[1, 2], 1, tolerance = 1e-10)
    expect_equal(result[2, 1], 1, tolerance = 1e-10)
})

test_that('compare_betas: identical topics have Hellinger distance 0', {
    result = compare_betas(beta_identical, vocab = c('a', 'b'))
    expect_equal(max(abs(result)), 0, tolerance = 1e-10)
})

test_that('compare_betas: cross-model comparison returns k1 x k2 matrix', {
    result = compare_betas(beta_disjoint, beta2 = beta_3topics, vocab = vocab)
    expect_equal(dim(result), c(2L, 3L))
})
