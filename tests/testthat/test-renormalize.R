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

test_that('expected_entropy: result is bounded by [0, log2(k)]', {
    k = 4
    result = expected_entropy(1, k = k)
    expect_gte(result, 0)
    expect_lte(result, log2(k))
})

test_that('expected_entropy: large alpha approaches log2(k)', {
    k = 8
    expect_equal(expected_entropy(1e6, k = k), log2(k), tolerance = 0.01)
})

test_that('expected_entropy: asymmetric alpha vector works without error', {
    expect_no_error(expected_entropy(c(0.5, 1.0, 2.0, 5.0)))
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

test_that('renorm: exponent < 1 spreads distributions (higher entropy)', {
    result = renorm(probs, group, p, exponent = 0.5)
    H_before_A = entropy(probs$p[probs$group == 'A'])
    H_before_B = entropy(probs$p[probs$group == 'B'])
    H_after_A  = entropy(result$p[result$group == 'A'])
    H_after_B  = entropy(result$p[result$group == 'B'])
    expect_gt(H_after_A, H_before_A)
    expect_gt(H_after_B, H_before_B)
})

test_that('renorm: keep_original = TRUE adds p_rn column, preserves p', {
    result = renorm(probs, group, p, exponent = 2, keep_original = TRUE)
    expect_true('p_rn' %in% names(result))
    expect_equal(result$p, probs$p)
})

test_that('renorm: keep_original = TRUE names new column after p_col', {
    probs2 = dplyr::rename(probs, p_doc = p)
    result = renorm(probs2, group, p_doc, exponent = 2, keep_original = TRUE)
    expect_true('p_doc_rn' %in% names(result))
    expect_equal(result$p_doc, probs2$p_doc)
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

test_that('solve_power: beta < 1 when target entropy exceeds current entropy', {
    p = c(0.7, 0.2, 0.1)
    target_H = entropy(p) + 0.3
    beta = solve_power(p, target_H)
    expect_lt(beta, 1)
})

test_that('solve_power: return_full = TRUE returns a list', {
    result = solve_power(c(0.5, 0.3, 0.2), 1.0, return_full = TRUE)
    expect_true(is.list(result))
})

test_that('solve_power: always returns a numeric (purrr::possibly guards against uniroot errors)', {
    ## Even for an unreachable target_H, uniroot extends the interval rather
    ## than erroring, so the result is a finite-or-large numeric, not NA
    p = c(0.5, 0.3, 0.2)
    result = suppressWarnings(solve_power(p, target_H = 10))
    expect_true(is.numeric(result))
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

test_that('target_power: target below mean entropy returns power > 1', {
    result = target_power(probs, group, p, target_entropy = 0.5)
    expect_gt(result, 1)
})

test_that('target_power: warns when >10% of powers cannot be solved', {
    ## Point-mass groups have entropy = 0 for all beta, so solve_power fails
    ## when target_H > 0; 2 of 5 groups fail → 40% > 10% → warning
    many_groups = tibble::tibble(
        group = c(rep('ok1', 3), rep('ok2', 3), rep('ok3', 3),
                  rep('bad1', 3), rep('bad2', 3)),
        p     = c(0.5, 0.3, 0.2,
                  0.5, 0.3, 0.2,
                  0.5, 0.3, 0.2,
                  1.0, 0.0, 0.0,
                  1.0, 0.0, 0.0)
    )
    expect_warning(
        target_power(many_groups, group, p, target_entropy = 1.0),
        regexp = '10%'
    )
})
