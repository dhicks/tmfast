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
    warning('Not yet tested')
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
    warning('Not yet tested')
})
