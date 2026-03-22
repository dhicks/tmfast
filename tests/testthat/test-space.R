## Fixture: fitted tmfast model (same construction as test-hellinger.R)
set.seed(20260322)
n_docs  = 50L
n_words = 30L
dtm_mx = matrix(rpois(n_docs * n_words, lambda = 3), nrow = n_docs) |>
    magrittr::set_rownames(paste0('doc',  seq_len(n_docs))) |>
    magrittr::set_colnames(paste0('word', seq_len(n_words))) |>
    Matrix::Matrix(sparse = TRUE)
fitted = tmfast(dtm_mx, n = 3)

## ---- umap() -----------------------------------------------------------------

test_that('umap.tmfast: returns dataframe with correct columns and rows', {
    skip_if_not_installed('umap')
    result = umap(fitted, k = 3)
    expect_named(result, c('document', 'x', 'y'))
    expect_equal(nrow(result), n_docs)
})

test_that('umap.tmfast: df = FALSE returns umap object', {
    skip_if_not_installed('umap')
    result = umap(fitted, k = 3, df = FALSE)
    expect_s3_class(result, 'umap')
})

test_that('umap.tmfast: include_data controls data slot', {
    skip_if_not_installed('umap')
    without_data = umap(fitted, k = 3, df = FALSE)
    with_data    = umap(fitted, k = 3, df = FALSE, include_data = TRUE)
    expect_null(without_data$data)
    expect_false(is.null(with_data$data))
})
