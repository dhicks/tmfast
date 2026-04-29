## ---- entropy() --------------------------------------------------------------

test_that('entropy: uniform distributions achieve maximum entropy', {
    expect_equal(entropy(c(0.5, 0.5)),   1)
    expect_equal(entropy(rep(1/4, 4)),   2)
})

test_that('entropy: point mass achieves zero entropy', {
    expect_equal(entropy(c(1, 0)), 0)   # 0*log(0) handled via na.rm
})

test_that('entropy: base argument changes units', {
    expect_equal(entropy(c(0.5, 0.5), base = exp(1)), log(2), tolerance = 1e-10)
})

test_that('entropy: error when probabilities do not sum to 1', {
    expect_error(entropy(c(0.5, 0.6)))
})

## ---- ndH() ------------------------------------------------------------------

## 3 documents, 2 terms
## 'focused': appears only in doc A → maximum info gain
## 'uniform': appears equally in all 3 docs → zero info gain
corpus = tibble::tribble(
    ~doc, ~term,     ~n,
    'A',  'focused', 10,
    'A',  'uniform', 10,
    'B',  'uniform', 10,
    'C',  'uniform', 10
)

test_that('ndH: output has correct column names', {
    result = ndH(corpus, doc, term, n)
    expect_named(result, c('term', 'H', 'dH', 'n', 'ndH'))
})

test_that('ndH: output is sorted descending by ndH', {
    result = ndH(corpus, doc, term, n)
    expect_equal(result$ndH, sort(result$ndH, decreasing = TRUE))
})

test_that('ndH: uniformly distributed term has ndH = 0', {
    result = ndH(corpus, doc, term, n)
    ndH_uniform = result$ndH[result$term == 'uniform']
    expect_equal(ndH_uniform, 0, tolerance = 1e-10)
})

test_that('ndH: focused term matches expected value', {
    ## p = c(1), H = 0, dH = log2(3), n = 10, ndH = log2(10) * log2(3)
    result = ndH(corpus, doc, term, n)
    ndH_focused = result$ndH[result$term == 'focused']
    expect_equal(ndH_focused, log2(10) * log2(3), tolerance = 1e-10)
})

## ---- ndR() ------------------------------------------------------------------

## Equal-length documents (30 words each): r_i = 1/3 for all docs
## 'uniform' spread equally → p_i = r_i → dR = 0 → ndR = 0
equal_corpus = tibble::tribble(
    ~doc, ~term,     ~n,
    'A',  'uniform', 10,
    'B',  'uniform', 10,
    'C',  'uniform', 10,
    'A',  'focused', 29,
    'B',  'focused',  1,
    'C',  'focused',  1
)

test_that('ndR: output has correct column names', {
    result = ndR(equal_corpus, doc, term, n)
    expect_named(result, c('term', 'n', 'dR', 'ndR'))
})

test_that('ndR: output is sorted descending by ndR', {
    result = ndR(equal_corpus, doc, term, n)
    expect_equal(result$ndR, sort(result$ndR, decreasing = TRUE))
})

test_that('ndR: works when count column is not named n', {
    renamed = dplyr::rename(equal_corpus, count = n)
    result = ndR(renamed, doc, term, count)
    expect_named(result, c('term', 'n', 'dR', 'ndR'))
    expect_true(all(result$ndR >= 0))
})

test_that('ndR: focused term has higher ndR than uniformly spread term', {
    result = ndR(equal_corpus, doc, term, n)
    ndR_focused = result$ndR[result$term == 'focused']
    ndR_uniform = result$ndR[result$term == 'uniform']
    expect_gt(ndR_focused, ndR_uniform)
    expect_true(all(result$ndR >= 0))  # KL divergence is non-negative
})
