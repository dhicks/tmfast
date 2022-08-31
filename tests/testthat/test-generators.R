test_that('peak_alpha', {
    expect_equal(peak_alpha(5, 2), c(0.05, 0.80, 0.05, 0.05, 0.05))
    expect_equal(peak_alpha(8, 3, peak = .3, scale = 10),
                 c(1, 1, 3, 1, 1, 1, 1, 1))
})

test_that('rdirichlet', {
    alpha_i = c(1, 3, 5)
    sample = rdirichlet(1500, alpha_i)
    expect_identical(dim(sample), c(1500L, length(alpha_i)))
    expect_equal(rowSums(sample), rep(1, 1500))
    ## Expected value of components
    alpha0 = sum(alpha_i)
    expect_equal(colMeans(sample),
                 alpha_i / alpha0,
                 tolerance = .05)
    ## Variance of components
    expect_equal(apply(sample, 2, var),
                 alpha_i * (alpha0 - alpha_i) / (alpha0^3 + alpha0),
                 tolerance = .05)
})

test_that('draw_words', {
    ## Topic-doc distribution, 5 topics
    theta = peak_alpha(5, 1)
    ## Word-topic distribution, 20 words x 5 topics
    phi = rdirichlet(5, 1, k = 20)
    sample = draw_words(5000, theta, phi)
    expect_equal(dim(sample), c(20, 2))
    expect_named(sample, c('word', 'n'))
    ## Marginal probability for each word
    marginals = sample |>
        dplyr::mutate(frac = n / sum(n)) |>
        dplyr::pull(frac)
    expect_equal(colSums(theta*phi), marginals, tolerance = .05)
})
