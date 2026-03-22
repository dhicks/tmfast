library(tidyverse)
library(tictoc)
library(here)
library(furrr)
plan(multisession, workers = 6)

library(tmfast)
library(stm)
library(tidytext)
library(memoise)


## Number of topics and documents
k = 40         # topics
Mj = 100       # docs for each topic
M = Mj * k     # total docs
vocab = M

## Simulation parameters ----
## Negative binomial distribution of doc lengths
size = 10             # Size and mean
mu = 300
sqrt(mu + mu^2/size)  # Resulting SD of document sizes

## Dirichlet distributions for topic-docs and word-topics
topic_peak = .8
topic_scale = 10

word_beta = 0.1

## Generate corpus ----
## Seed
set.seed(2022-11-10)

## True topic-doc distribution
theta = purrr::map(1:k,
                   ~rdirichlet(Mj, peak_alpha(k, .x,
                                              peak = topic_peak,
                                              scale = topic_scale))) %>%
    do.call(rbind, .)
theta_df = theta |>
    tibble::as_tibble(rownames = 'doc',
                      .name_repair = tmfast:::make_colnames) |>
    dplyr::mutate(doc = as.integer(doc)) |>
    tidyr::pivot_longer(tidyselect::starts_with('V'),
                        names_to = 'topic',
                        values_to = 'prob')
## phi_j:  Word distribution for topic j
phi = rdirichlet(k, word_beta, k = vocab)

## N_i:  Length of document i
N = rnbinom(M, size = size, mu = mu)

## Draw corpus
## ~110 seconds w/o caching
draw_corpus = memoise(draw_corpus,
                      cache = cachem::cache_disk(dir = here('data',
                                                            'profiling_cache')))
tic()
corpus = draw_corpus(N, theta, phi) |>
    cast_sparse(doc, word, n)
toc()
# dtm = dplyr::mutate(corpus, n = log1p(n))

# re_size = 500
# fit_stm = FALSE
## Profiling ----
fit_re = function(i,
                  re_size = 500,
                  beta_target = expected_entropy(word_beta, k = vocab),
                  gamma_target = expected_entropy(
                      peak_alpha(k,
                                 1,
                                 peak = topic_peak,
                                 scale = topic_scale)),
                  fit_stm = TRUE) {
    # message(i)

    ## 1. resample 500 docs from the corpus
    docs = sample.int(M, size = re_size, replace = TRUE)
    corpus_sample = corpus[docs,]

    ## 2. fit topic models to resample
    ## 2.a. tmfast
    tic()
    fitted_tmf = corpus_sample |>
        log1p() |>
        tmfast(n = k)
    time_tmf = toc(quiet = TRUE)
    time_tmf = time_tmf$toc - time_tmf$tic
    names(time_tmf) = NULL

    ## 2.b. STM
    if (fit_stm) {
        tic()
        fitted_stm = corpus_sample |>
            stm(K = k, verbose = FALSE)
        time_stm = toc(quiet = TRUE)
        time_stm = time_stm$toc - time_stm$tic
        names(time_stm) = NULL
    }

    ## 3. match topics and calculate Hellinger distance
    ## 3.a. tmfast
    beta_power = tidy(fitted_tmf, k, 'beta') |>
        target_power(topic, beta, beta_target)
    beta_tmf = tidy(fitted_tmf, k, 'beta', exponent = beta_power) |>
        ## Fix order of words
        dplyr::mutate(token = as.integer(token)) |>
        dplyr::arrange(token) |>
        ## And dropped words
        tidyr::complete(token = 1:vocab) |>
        tidyr::pivot_wider(names_from = 'topic',
                           values_from = 'beta', values_fill = 0,
                           names_sort = TRUE) |>
        dplyr::select(-any_of('NA')) |>
        ## Coerce to matrix
        tibble::column_to_rownames('token') |>
        as.matrix()
    dist_tmf = hellinger(phi, t(beta_tmf))
    soln_tmf = lpSolve::lp.assign(dist_tmf)
    word_accuracy_tmf = hellinger(phi, soln_tmf$solution %*% t(beta_tmf)) |>
        diag()


    gamma_power = tidy(fitted_tmf, k, 'gamma') |>
        target_power(document, gamma, gamma_target)
    gamma_tmf = tidy(fitted_tmf, k, 'gamma', exponent = gamma_power) |>
        cast_sparse(document, topic, gamma) |>
        as.matrix() |>
        magrittr::extract(as.character(docs),)
    assertthat::assert_that(all.equal(rownames(gamma_tmf),
                                      as.character(docs)))
    topic_accuracy_tmf = hellinger(theta[docs,],
                                   gamma_tmf %*% t(soln_tmf$solution)) |>
        diag()

    ## 3.b. STM
    if (fit_stm) {
        beta_stm = tidy(fitted_stm, 'beta') |>
            ## Fix order of words
            dplyr::rename(token = term) |>
            dplyr::mutate(token = as.integer(token)) |>
            dplyr::arrange(token) |>
            ## And dropped words
            tidyr::complete(token = 1:vocab) |>
            tidyr::pivot_wider(names_from = 'topic',
                               values_from = 'beta', values_fill = 0,
                               names_sort = TRUE) |>
            dplyr::select(-any_of('NA')) |>
            ## Coerce to matrix
            tibble::column_to_rownames('token') |>
            as.matrix()
        dist_stm = hellinger(phi, t(beta_stm))
        soln_stm = lpSolve::lp.assign(dist_stm)
        word_accuracy_stm = hellinger(phi, soln_stm$solution %*% t(beta_stm)) |>
            diag()

        gamma_stm = tidy(fitted_stm, 'gamma') |>
            cast_sparse(document, topic, gamma) |>
            as.matrix() |>
            magrittr::set_rownames(docs)
        assertthat::assert_that(all.equal(rownames(gamma_stm),
                                          as.character(docs)))
        topic_accuracy_stm = hellinger(theta[docs,],
                                       gamma_stm %*% t(soln_stm$solution)) |>
            diag()
    }

    # 4. report time and H dist
    to_return = tibble(i,
                       time_tmf,
                       word_accuracy_tmf = mean(word_accuracy_tmf),
                       topic_accuracy_tmf = mean(topic_accuracy_tmf))
    if (fit_stm) {
        to_return = bind_cols(to_return,
                              tibble(time_stm,
                                     word_accuracy_stm = mean(word_accuracy_stm),
                                     topic_accuracy_stm = mean(topic_accuracy_stm)))
    }
    return(to_return)
}
# debugonce(fit_re)
# foo = fit_re(50, fit_stm = FALSE)


## w/o parallelization: ~90 sec for 5
## so ~30 min for 100
## w/ 4 workers, ~20 min
tic()
profiles = future_map_dfr(1:20, fit_re, fit_stm = TRUE,
                          .options = furrr_options(seed = TRUE),
                          .progress = TRUE)
toc()

## Plots ----
## Ratio of wall time to fit
## tmfast is about 20x faster than STM
ggplot(profiles, aes(x = i, y = time_stm / time_tmf)) +
    geom_point() +
    geom_hline(yintercept = 1)

profiles |>
    mutate(ratio = time_stm / time_tmf) |>
    pull(ratio) |>
    mean()

## Hellinger distance for word-topic distributions (lower is better)
## TMF is slightly better
ggplot(profiles, aes(x = i)) +
    geom_linerange(aes(ymin = word_accuracy_tmf,
                       ymax = word_accuracy_stm), alpha = .5) +
    geom_point(aes(y = word_accuracy_tmf, color = 'tmfast')) +
    geom_point(aes(y = word_accuracy_stm, color = 'stm'))

## Hellinger distance for topic-document distributions (lower is better)
## STM is somewht better
ggplot(profiles, aes(x = i)) +
    geom_linerange(aes(ymin = topic_accuracy_tmf,
                       ymax = topic_accuracy_stm), alpha = .5) +
    geom_point(aes(y = topic_accuracy_tmf, color = 'tmfast')) +
    geom_point(aes(y = topic_accuracy_stm, color = 'stm'))

profiles |>
    summarize(across(matches('accuracy'), mean))
