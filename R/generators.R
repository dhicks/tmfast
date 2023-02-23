#' Alpha parameter with a single peak
#'
#' This function allows us to quickly define an `alpha` parameter for a Dirichlet distribution with a single (presumably high) `peak*scale` value at component `i` and all other components a uniform (presumably low) value `(1-peak)/(k-1)*scale`.
#'
#' @param k Number of components
#' @param i Index for the component that takes value `peak`
#' @param peak Value for the single peak component
#' @export
#' @return Vector of length `k`
#' @family generators
peak_alpha = function(k, i, peak = .8, scale = 1) {
    alpha = rep((1 - peak)/(k-1), k)
    alpha[i] = peak
    alpha = scale * alpha
    return(alpha)
}

#' Sample from the Dirichlet distribution
#'
#' @param n Number of samples (rows) to draw
#' @param alpha Concentration parameters; either length 1 or length > 1
#'   If length 1, assumes symmetric Dirichlet; `k` must not be null
#' @param k Number of components (columns); ignored if `length(alpha) > 1`
#' @return A matrix of `n` rows and `length(alpha)` or `k` columns
#' @examples
#'   rdirichlet(10, .1, 5)
#'   rdirichlet(10, c(.8, .1, .1))
#' @export
#' @family generators
rdirichlet = function (n, alpha, k = NULL)
{
    if (identical(length(alpha), 1L)) {
        alpha = rep(alpha, k)
    }
    l <- length(alpha)
    x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
    sm <- x %*% rep(1, l)
    x/as.vector(sm)
}

#' Draw a single word given topic and word distributions
#'
#' @param theta Topic distribution, length \eqn{k} vector
#' @param phi Word distribution for all topics, \eqn{k \times v} matrix
#' @return Integer representation for a single word
draw_a_word = function(theta, phi) {
    z = sample.int(length(theta), size = 1, prob = theta)
    w = sample.int(ncol(phi), size = 1, prob = phi[z,])
    return(w)
}

#' Draw words for one document
#'
#' @param Ni length of document i
#' @param theta `theta_i`, topic distribution for document i
#' @param phi word distribution for all topics
#' @return Document-term matrix (single document), as a tibble, with columns `word` and `n`
draw_words = function(Ni, theta, phi) {
    word = purrr::map_int(1:Ni, ~ draw_a_word(theta, phi))
    tibble::tibble(word) |>
        dplyr::count(word)
}

#' Draw a collection of documents
#' @param N Length of documents
#' @param theta Topic distribution for all documents, \eqn{n \times k} matrix
#' @param phi Word distribution for all topics, \eqn{k \times v} matrix
#' @return Document-term matrix, as a tibble, with columns `doc`, `word`, and `n`
#' @export
#' @family generators
draw_corpus = function(N, theta, phi) {
    furrr::future_map_dfr(1:length(N),
                   ~ draw_words(N[.x], theta[.x,], phi),
                   .id = 'doc',
                   .options = furrr::furrr_options(seed = TRUE),
                   .progress = TRUE)|>
        dplyr::mutate(doc = as.integer(doc))
}

#' @import magrittr
NULL
#> NULL

#' "Journal-specific" simulation scenario
#'
#' Generates a corpus with `Mj` documents from `k` journals, each of which has a characteristic topic.  Fits a varimax topic model of rank `k`, rotates the word-topic distribution to align with the true values, and reports Hellinger distance comparisons for each topic (word-topic) and document (topic-doc).
#' @param k Number of topics/journals
#' @param Mj Number of documents from each journal
#' @param {topic_peak,topic_scale} Parameters for the asymmetric Dirichlet prior for true topic-doc distributions
#' @param word_beta Parameter for the symmetric Dirichlet prior for true word-doc distributions
#' @param vocab Size of the vocabulary
#' @param {size,mu} Parameters for the negative binomial distribution of document lengths
#' @param bigjournal Should the first journal have documents 10x as long (on average) as the others?
#' @param verbose When TRUE, sends messages about the progress of the simulation
#' @export
#' @family generators
journal_specific = function(k = 5,
                            Mj = 100,
                            topic_peak = .8, topic_scale = 10,
                            word_beta = .01,
                            vocab = 10*Mj*k,
                            size = 3, mu = 300,
                            bigjournal = FALSE,
                            verbose = TRUE) {
    M = k*Mj
    ## True topic-doc distribution
    theta = purrr::map(1:k,
                       ~rdirichlet(Mj, peak_alpha(k, .x,
                                                  peak = topic_peak,
                                                  scale = topic_scale))) %>%
        do.call(rbind, .)
    theta_df = theta |>
        tibble::as_tibble(rownames = 'doc', .name_repair = make_colnames) |>
        dplyr::mutate(doc = as.integer(doc)) |>
        tidyr::pivot_longer(tidyselect::starts_with('V'),
                            names_to = 'topic',
                            values_to = 'prob')
    ## phi_j:  Word distribution for topic j
    phi = rdirichlet(k, word_beta, k = vocab)
    ## N_i:  Length of document i
    N = rnbinom(M, size = size, mu = mu)
    if (bigjournal) {
        N[1:Mj] = 10*N[1:Mj]
    }
    ## Draw corpus
    if (verbose) message('Drawing corpus')
    corpus = draw_corpus(N, theta, phi)
    dtm = dplyr::mutate(corpus, n = log1p(n))

    ## Fit tm
    if (verbose) message('Fitting topic model')
    fitted = tmfast(dtm, k)
    ## beta: fitted varimax loadings, transformed to probability distributions
    beta = tidy(fitted, k, 'beta')
    ## Hellinger distance of word-topic distributions
    beta_mx = beta |>
        ## Fix order of words
        dplyr::mutate(token = as.integer(token)) |>
        dplyr::arrange(token) |>
        ## And dropped words
        tidyr::complete(token = 1:vocab) |>
        tidyr::pivot_wider(names_from = 'topic',
                           values_from = 'beta', values_fill = 0,
                           names_sort = TRUE) |>
        dplyr::select(-`NA`) |>
        ## Coerce to matrix
        tibble::column_to_rownames('token') |>
        as.matrix()
    ## Use lpSolve to match fitted topics to true topics
    dist = hellinger(phi, t(beta_mx))
    soln = lpSolve::lp.assign(dist)
    phi_accuracy = hellinger(phi, soln$solution %*% t(beta_mx)) |>
        diag()

    ## Topic-doc distributions
    gamma_df = tidy(fitted, k, 'gamma', rotation = soln$solution)

    theta_accuracy = hellinger(theta_df, id1 = doc, prob1 = prob,
                               topicsdf2 = gamma_df, id2 = doc, prob2 = gamma,
                               df = TRUE) |>
        dplyr::filter(doc_x == doc_y) |>
        dplyr::pull(dist)

    tibble::tibble(phi = mean(phi_accuracy),
                   phi_vec = list(phi_accuracy),
                   theta = mean(theta_accuracy),
                   theta_vec = list(theta_accuracy))
}
