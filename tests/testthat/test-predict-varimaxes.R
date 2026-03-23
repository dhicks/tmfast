## Fixture: block-diagonal DTM with two clear topics
## Block structure ensures varimax factors load positively on their respective terms,
## satisfying the positive-skew assertion without disabling it
set.seed(20260323)
block_high = matrix(rpois(100, 20), nrow = 10, ncol = 10)
block_low  = matrix(rpois(100,  1), nrow = 10, ncol = 10)
dtm = rbind(cbind(block_high, block_low),
            cbind(block_low,  block_high))
colnames(dtm) = paste0('t', 1:20)
rownames(dtm) = paste0('doc', 1:20)
fitted = varimax_irlba(dtm, n = 2)

## ---- predict.varimaxes() ----------------------------------------------------

test_that('predict.varimaxes: returns matrix with correct dimensions', {
    result = predict(fitted, dtm)
    expect_true(is.matrix(result))
    expect_equal(dim(result), c(nrow(dtm), max(fitted$n)))
})

test_that('predict.varimaxes: projecting training data recovers varimax scores', {
    k = max(fitted$n)
    pc_scores = predict(fitted, dtm)
    reproduced = scale(pc_scores[, 1:k]) %*% rotation(fitted, k)
    expect_equal(reproduced, scores(fitted, k), tolerance = 1e-10,
                 ignore_attr = TRUE)
})

test_that('predict.varimaxes: error when column names do not match vocabulary', {
    bad = dtm[, 1:3]  # only 3 of 20 columns
    expect_error(predict(fitted, bad), "named columns")
})

test_that('predict.varimaxes: error when extra dots passed', {
    expect_error(predict(fitted, dtm, extra = 1))
})
