## Fixture: fitted tmfast model and tidied gamma dataframe
set.seed(20260322)
n_docs  = 50L
n_words = 30L
dtm_mx = matrix(rpois(n_docs * n_words, lambda = 3), nrow = n_docs) |>
    magrittr::set_rownames(paste0('doc',  seq_len(n_docs))) |>
    magrittr::set_colnames(paste0('word', seq_len(n_words))) |>
    Matrix::Matrix(sparse = TRUE)
fitted   = tmfast(dtm_mx, n = 3)
gamma_df = tidy(fitted, k = 3, matrix = 'gamma')
## gamma_df has columns: document, topic, gamma
## hellinger.data.frame() defaults to prob1 = 'prob', so always pass prob1 = 'gamma'

test_that('Hellinger distance for matrices', {
    mat1 = rdirichlet(100, peak_alpha(5, 1), 5)
    expect_equal(diag(hellinger(mat1)), rep(0, 100))
    expect_equal(diag(hellinger(mat1, mat1)), rep(0, 100))

    mat2 = c(peak_alpha(6, 1, peak = .5),
             peak_alpha(6, 2, peak = .5),
             peak_alpha(6, 3, peak = .5),
             peak_alpha(6, 4, peak = .5),
             peak_alpha(6, 5, peak = .5),
             peak_alpha(6, 6, peak = .5)) |>
        matrix(nrow = 6, ncol = 6)
    dist1 = hellinger(mat2)
    dist2 = hellinger(mat2, mat2)
    dist3 = matrix(rep(sqrt(1 - (2*sqrt(.5)*sqrt(.1) + 4*sqrt(.1)*sqrt(.1))),
                       6*6),
                   nrow = 6, ncol = 6) |>
        `diag<-`(0)
    expect_equal(dist1, dist2)
    expect_equal(dist1, dist3)
    expect_equal(dist2, dist3)
})

test_that('Hellinger distance for data frames', {
    result = hellinger(gamma_df, prob1 = 'gamma')

    ## Square matrix with correct dimensions
    expect_equal(dim(result), c(n_docs, n_docs))

    ## Self-distances are 0
    expect_equal(diag(result), rep(0, n_docs), ignore_attr = TRUE)

    ## All values in [0, 1]
    expect_true(min(result) >= 0)
    expect_true(max(result) <= 1)

    ## Symmetric
    expect_equal(result, t(result))

    ## Cross-distance: two disjoint halves → non-square result
    docs1 = paste0('doc', 1:25)
    docs2 = paste0('doc', 26:50)
    half1 = dplyr::filter(gamma_df, document %in% docs1)
    half2 = dplyr::filter(gamma_df, document %in% docs2)
    cross = hellinger(half1, prob1 = 'gamma', topicsdf2 = half2, prob2 = 'gamma')
    expect_equal(dim(cross), c(25L, 25L))

    ## df = TRUE → tidy dataframe; id cols renamed (both default to 'document')
    result_df = hellinger(gamma_df, prob1 = 'gamma', df = TRUE)
    expect_named(result_df, c('document_x', 'document_y', 'dist'))

    ## Consistency with matrix method
    mx = build_matrix(gamma_df, document, topic, gamma, sparse = FALSE)
    expect_equal(result, hellinger(mx))
})

test_that('check_dots_empty: hellinger.Matrix rejects unexpected args', {
    mat1 = rdirichlet(10, peak_alpha(3, 1), 3)
    expect_error(hellinger(mat1, typo = TRUE), class = 'rlib_error_dots_nonempty')
})

test_that('check_dots_empty: hellinger.data.frame rejects unexpected args', {
    set.seed(20260322)
    topics = rdirichlet(10, peak_alpha(3, 1), 3) |>
        tibble::as_tibble(.name_repair = \(x) paste0('T', seq_along(x))) |>
        tibble::add_column(document = paste0('doc', 1:10), .before = 1) |>
        tidyr::pivot_longer(-document, names_to = 'topic', values_to = 'gamma')
    expect_error(hellinger(topics, id1 = document, prob1 = gamma, typo = TRUE),
                 class = 'rlib_error_dots_nonempty')
})

test_that('t-SNE wrapper', {
    skip_if_not_installed('Rtsne')
    set.seed(20260322)
    result = tsne(fitted, k = 3)
    expect_named(result, c('document', 'x', 'y'))
    expect_equal(nrow(result), n_docs)

    set.seed(20260322)
    result_raw = tsne(fitted, k = 3, df = FALSE)
    expect_s3_class(result_raw, 'Rtsne')
})
