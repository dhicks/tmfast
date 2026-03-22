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

## ---- expected_entropy() -----------------------------------------------------

test_that('expected_entropy: error when scalar alpha given without k', {
    expect_error(expected_entropy(1))
})

test_that('expected_entropy: scalar alpha expands to symmetric Dirichlet', {
    expect_equal(expected_entropy(1, k = 4), expected_entropy(c(1, 1, 1, 1)))
})

test_that('expected_entropy: higher concentration → higher expected entropy', {
    ## Large alpha → samples cluster near uniform → high entropy
    ## Small alpha → samples cluster near vertices → low entropy
    expect_gt(expected_entropy(10, k = 4), expected_entropy(0.1, k = 4))
})

## ---- renorm() ---------------------------------------------------------------

probs = tibble::tibble(
    group = c('A', 'A', 'A', 'B', 'B', 'B'),
    p     = c(0.5, 0.3, 0.2, 0.6, 0.3, 0.1)
)

test_that('renorm: exponent = 1 is identity', {
    result = renorm(probs, group, p, exponent = 1)
    expect_equal(result$p, probs$p, tolerance = 1e-10)
})

test_that('renorm: exponent > 1 concentrates distributions (lower entropy)', {
    result = renorm(probs, group, p, exponent = 3)
    H_before_A = entropy(probs$p[probs$group == 'A'])
    H_before_B = entropy(probs$p[probs$group == 'B'])
    H_after_A  = entropy(result$p[result$group == 'A'])
    H_after_B  = entropy(result$p[result$group == 'B'])
    expect_lt(H_after_A, H_before_A)
    expect_lt(H_after_B, H_before_B)
})

test_that('renorm: keep_original = TRUE adds p_rn column, preserves p', {
    result = renorm(probs, group, p, exponent = 2, keep_original = TRUE)
    expect_true('p_rn' %in% names(result))
    expect_equal(result$p, probs$p)
})

test_that('renorm: output sums to 1 per group', {
    result = renorm(probs, group, p, exponent = 2) |>
        dplyr::group_by(group) |>
        dplyr::summarize(s = sum(p))
    expect_equal(result$s, c(1, 1), tolerance = 1e-10)
})

## ---- solve_power() ----------------------------------------------------------

test_that('solve_power: returns exponent ~1 when target equals current entropy', {
    p = c(0.5, 0.3, 0.2)
    beta = solve_power(p, entropy(p))
    expect_equal(beta, 1, tolerance = 0.01)
})

test_that('solve_power: applying solved exponent achieves target entropy', {
    p = c(0.5, 0.3, 0.2)
    target_H = 1.0
    beta = solve_power(p, target_H)
    p_rn = p^beta / sum(p^beta)
    expect_equal(entropy(p_rn), target_H, tolerance = 0.01)
})

test_that('solve_power: return_full = TRUE returns a list', {
    result = solve_power(c(0.5, 0.3, 0.2), 1.0, return_full = TRUE)
    expect_true(is.list(result))
})

## ---- target_power() ---------------------------------------------------------

test_that('target_power: returns a scalar', {
    result = target_power(probs, group, p, target_entropy = 1.0)
    expect_true(is.numeric(result) && length(result) == 1L)
})

test_that('target_power: targeting mean current entropy returns power ~1', {
    H_A = entropy(probs$p[probs$group == 'A'])
    H_B = entropy(probs$p[probs$group == 'B'])
    target_H = mean(c(H_A, H_B))
    result = target_power(probs, group, p, target_entropy = target_H)
    expect_equal(result, 1, tolerance = 0.15)
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
## Note: ndR() has a hardcoded 'n' bug in totals calculation;
##       count column must be named 'n' for ndR() to work correctly
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

test_that('ndR: focused term has higher ndR than uniformly spread term', {
    result = ndR(equal_corpus, doc, term, n)
    ndR_focused = result$ndR[result$term == 'focused']
    ndR_uniform = result$ndR[result$term == 'uniform']
    expect_gt(ndR_focused, ndR_uniform)
    expect_true(all(result$ndR >= 0))  # KL divergence is non-negative
})
