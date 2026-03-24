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

## ---- draw_corpus() ----------------------------------------------------------

test_that('draw_corpus: returns tibble with correct columns and integer doc IDs', {
    set.seed(20260324)
    theta = rdirichlet(3, peak_alpha(4, 1), 4)  # 3 docs, 4 topics
    phi   = rdirichlet(4, 1, k = 10)             # 4 topics, 10 words
    N     = c(100L, 200L, 150L)
    result = draw_corpus(N, theta, phi)
    expect_named(result, c('doc', 'word', 'n'))
    expect_true(is.integer(result$doc))
    expect_equal(sort(unique(result$doc)), 1:3)
})

test_that('draw_corpus: total words per doc equals N', {
    set.seed(20260324)
    theta = rdirichlet(3, peak_alpha(4, 1), 4)
    phi   = rdirichlet(4, 1, k = 10)
    N     = c(100L, 200L, 150L)
    result = draw_corpus(N, theta, phi)
    totals = result |>
        dplyr::group_by(doc) |>
        dplyr::summarise(total = sum(n)) |>
        dplyr::arrange(doc)
    expect_equal(totals$total, N)
})

## ---- journal_specific() -----------------------------------------------------

test_that('journal_specific: returns tibble with expected columns and valid values', {
    skip_if_not_installed('lpSolve')
    set.seed(20260324)
    result = journal_specific(k = 2, Mj = 10, vocab = 50, verbose = FALSE)
    expect_named(result, c('phi', 'phi_vec', 'theta', 'theta_vec'))
    expect_true(is.numeric(result$phi))
    expect_true(is.numeric(result$theta))
    expect_true(result$phi >= 0 && result$phi <= 1)
    expect_true(result$theta >= 0 && result$theta <= 1)
    expect_equal(length(result$phi_vec[[1]]),   2L)   # one distance per topic
    expect_equal(length(result$theta_vec[[1]]), 2L * 10L)  # one per doc
})
