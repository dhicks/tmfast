## Fixture: small DTM as sparse Matrix with doc/word names
## Using Poisson counts to mimic word frequencies; seed per project convention
set.seed(20260322)
n_docs  = 50L
n_words = 30L
dtm_mx = matrix(rpois(n_docs * n_words, lambda = 3), nrow = n_docs) |>
    magrittr::set_rownames(paste0('doc',  seq_len(n_docs))) |>
    magrittr::set_colnames(paste0('word', seq_len(n_words))) |>
    Matrix::Matrix(sparse = TRUE)

## Fitted model used across most tests
fitted = tmfast(dtm_mx, n = c(2, 3, 4))

## Long-format version of the same DTM (for testing dataframe input path)
dtm_long = dtm_mx |>
    as.matrix() |>
    tibble::as_tibble(rownames = 'doc') |>
    tidyr::pivot_longer(-doc, names_to = 'word', values_to = 'n') |>
    dplyr::filter(n > 0)

## ---- tmfast() ---------------------------------------------------------------

test_that('tmfast: output has correct classes', {
    expect_s3_class(fitted, 'tmfast')
    expect_s3_class(fitted, 'varimaxes')
})

test_that('tmfast: accepts a long dataframe', {
    fitted_df = tmfast(dtm_long, n = 3, row = doc, column = word, value = n)
    expect_s3_class(fitted_df, 'tmfast')
    expect_equal(fitted_df$n, 3)
    expect_named(fitted_df$varimax, '3')
})

test_that('tmfast: multi-k stored correctly', {
    expect_equal(fitted$n, c(2, 3, 4))
    expect_named(fitted$varimax, c('2', '3', '4'))
})

test_that('tmfast: retx controls storage of data matrix', {
    with_x    = tmfast(dtm_mx, n = 3, retx = TRUE)
    without_x = tmfast(dtm_mx, n = 3)
    expect_false(is.null(with_x$x))
    expect_true(is.null(without_x$x))
})

## ---- insert_topics() --------------------------------------------------------

test_that('insert_topics: returns unchanged when k already present', {
    result = insert_topics(fitted, 3)
    expect_equal(result$n, fitted$n)
    expect_identical(result$varimax[['3']], fitted$varimax[['3']])
})

test_that('insert_topics: error when k exceeds max', {
    expect_error(insert_topics(fitted, 5))
})

test_that('insert_topics: error when length(k) > 1', {
    expect_error(insert_topics(fitted, c(1, 2)))
})

test_that('insert_topics: error when data matrix unavailable', {
    expect_error(insert_topics(fitted, 1))
})

test_that('insert_topics: inserts new k with explicit x', {
    fitted_partial = tmfast(dtm_mx, n = c(3, 4))
    result = insert_topics(fitted_partial, 2, x = dtm_mx)
    expect_true(2 %in% result$n)
    expect_true('2' %in% names(result$varimax))
    expect_named(result$varimax[['2']], c('loadings', 'rotmat', 'scores'))
})

## ---- tidy.tmfast() beta branch ----------------------------------------------

test_that('tidy beta: returns expected columns', {
    beta = tidy(fitted, k = 3, matrix = 'beta')
    expect_named(beta, c('token', 'topic', 'beta'))
})

test_that('tidy beta: sums to 1 per topic', {
    beta = tidy(fitted, k = 3, matrix = 'beta')
    topic_sums = beta |>
        dplyr::group_by(topic) |>
        dplyr::summarise(s = sum(beta))
    expect_equal(topic_sums$s, rep(1, 3), tolerance = 1e-10)
})

test_that('tidy beta: all values non-negative', {
    beta = tidy(fitted, k = 3, matrix = 'beta')
    expect_true(min(beta$beta) >= 0)
})

test_that('tidy beta: correct number of topics', {
    beta = tidy(fitted, k = 3, matrix = 'beta')
    expect_equal(dplyr::n_distinct(beta$topic), 3)
})

test_that('tidy beta: df = FALSE returns a matrix', {
    expect_message(
        result <- tidy(fitted, k = 3, matrix = 'beta', df = FALSE),
        'not rotated'
    )
    expect_true(is.matrix(result))
})

## ---- tidy.tmfast() gamma branch ---------------------------------------------

test_that('tidy gamma: returns expected columns', {
    gamma = tidy(fitted, k = 3, matrix = 'gamma')
    expect_named(gamma, c('document', 'topic', 'gamma'))
})

test_that('tidy gamma: sums to 1 per document', {
    gamma = tidy(fitted, k = 3, matrix = 'gamma')
    doc_sums = gamma |>
        dplyr::group_by(document) |>
        dplyr::summarise(s = sum(gamma))
    expect_equal(doc_sums$s, rep(1, n_docs), tolerance = 1e-10)
})

test_that('tidy gamma: all values non-negative', {
    gamma = tidy(fitted, k = 3, matrix = 'gamma')
    expect_true(min(gamma$gamma) >= 0)
})

test_that('tidy gamma: correct number of topics', {
    gamma = tidy(fitted, k = 3, matrix = 'gamma')
    expect_equal(dplyr::n_distinct(gamma$topic), 3)
})

test_that('tidy gamma: df = FALSE returns a matrix', {
    expect_message(
        result <- tidy(fitted, k = 3, matrix = 'gamma', df = FALSE),
        'not rotated'
    )
    expect_true(is.matrix(result))
})

## ---- tidy_all() -------------------------------------------------------------

test_that('tidy_all beta: contains all k values', {
    out = tidy_all(fitted, matrix = 'beta')
    expect_equal(sort(unique(out$k)), c(2L, 3L, 4L))
})

test_that('tidy_all: k column is integer', {
    out = tidy_all(fitted, matrix = 'beta')
    expect_true(is.integer(out$k))
})

test_that('tidy_all gamma: works and has correct columns', {
    out = tidy_all(fitted, matrix = 'gamma')
    expect_true('k' %in% names(out))
    expect_true('gamma' %in% names(out))
    expect_equal(sort(unique(out$k)), c(2L, 3L, 4L))
})

## ---- S3 accessors: dimensions -----------------------------------------------

test_that('loadings: correct dimensions', {
    L = loadings(fitted, k = 3)
    expect_equal(nrow(L), n_words)
    expect_equal(ncol(L), 3)
})

test_that('scores: correct dimensions', {
    S = scores(fitted, k = 3)
    expect_equal(nrow(S), n_docs)
    expect_equal(ncol(S), 3)
})

test_that('rotation: square k x k matrix', {
    R = rotation(fitted, k = 3)
    expect_equal(dim(R), c(3, 3))
})
