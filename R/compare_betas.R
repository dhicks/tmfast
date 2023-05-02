## TODO: document this
compare_betas = function(beta1, beta2 = NULL, vocab) {
    fill = function(beta) {
        beta |>
            complete(token = vocab, topic, fill = list(beta = 0)) |>
            build_matrix(topic, token, beta) %>%
            .[, vocab]
    }
    beta1 = fill(beta1)
    if (is.null(beta2)) {
        beta2 = beta1
    } else {
        beta2 = fill(beta2)
    }

    assertthat::assert_that(all(colnames(beta1) == colnames(beta2)))

    hellinger(beta1, beta2)
}
