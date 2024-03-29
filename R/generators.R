#' Functions to generate document-term matrices

#' Alpha parameter with a single peak
#' 
#' This function allows us to quickly define an `alpha` parameter for a Dirichlet distribution with a single (presumably high) `peak*scale` value at component `i` and all other components a uniform (presumably low) value `(1-peak)/(k-1)*scale`. 
#' 
#' @param k Number of components 
#' @param i Index for the component that takes value `peak`
#' @param peak Value for the single peak component
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
rdirichlet<-function (n, alpha, k = NULL) 
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
#' @param theta Topic distribution
#' @param .phi Word distribution for all topics
#' @return Integer representation for a single word
draw_a_word = function(theta, phi) {
    z = sample.int(length(theta), size = 1, prob = theta)
    w = sample.int(ncol(phi), size = 1, prob = phi[z,])
    return(w)
}
# debugonce(draw_a_word)
# draw_a_word(c(.1, 1., .8))

#' Draw words for one document
#' 
#' @param Ni length of document i
#' @param theta `theta_i`, topic distribution for document i
#' @param .phi word distribution for all topics
#' @return Document-term matrix (single document), as a tibble, with columns `word` and `n`
#' @export
draw_words = function(Ni, theta, phi) {
    word = map_int(1:Ni, ~ draw_a_word(theta, phi))
    tibble(word) |> 
        count(word)
}
# draw_words(15, c(.1, .1, .8))
# draw_words(N_i[1], theta[1,])

#' Draw a collection of documents
#' @param N Length of documents
#' @param theta topic distribution for all documents
#' @param phi word distribution for all topics
#' @return Document-term matrix, as a tibble, with columns `doc`, `word`, and `n`
draw_corpus = function(N, theta, phi) {
    map_dfr(1:length(N), 
            ~draw_words(N[.x], theta[.x,], phi), 
            .id = 'doc') |> 
        mutate(doc = as.integer(doc))
}


#' "Journal-specific" simulation scenario
#' 
#' Generates a corpus with Mj documents from k journals, each of which has a characteristic topic.  Fits a varimax topic model of rank k, rotates the word-topic distribution to align with the true values, and reports Hellinger distance comparisons for each topic (word-topic) and document (topic-doc).  
#' @param k Number of topics/journals
#' @param Mj Number of documents from each journal
#' @param {topic_peak, topic_scale} Parameters for the asymmetric Dirichlet prior for true topic-doc distributions
#' @param word_beta Parameter for the symmetric Dirichlet prior for true word-doc distributions
#' @param vocab Size of the vocabulary
#' @param {size, mu} Parameters for the neg. binomial distribution of document lengths
#' @param bigjournal Should the first journal have documents 10x as long (on average) as the others? 
#' @param verbose When TRUE, sends messages about the progress of the simulation
#' @export 
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
    theta = map(1:k, 
                ~rdirichlet(Mj, peak_alpha(k, .x, 
                                           peak = topic_peak, 
                                           scale = topic_scale))) %>% 
        do.call(rbind, .)
    theta_df = theta |> 
        as_tibble(rownames = 'doc') |> 
        mutate(doc = as.integer(doc)) |> 
        pivot_longer(starts_with('V'), 
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
    dtm = mutate(corpus, n = log1p(n))
    
    ## Fit tm
    if (verbose) message('Fitting topic model')
    fitted = tmfast(dtm, k)
    ## beta: fitted varimax loadings, transformed to probability distributions
    beta = tidy(fitted, k, 'beta')
    ## Hellinger distance of word-topic distributions
    beta_mx = beta |> 
        ## Fix order of words
        mutate(token = as.integer(token)) |>
        arrange(token) |>
        ## And dropped words
        complete(token = 1:vocab) |>
        pivot_wider(names_from = 'topic',
                    values_from = 'beta', values_fill = 0,
                    names_sort = TRUE) |>
        select(-`NA`) |> 
        ## Coerce to matrix
        column_to_rownames('token') |>
        as.matrix()
    ## Use lpSolve to match fitted topics to true topics
    dist = hellinger(phi, t(beta_mx))
    soln = lpSolve::lp.assign(dist)
    phi_accuracy = hellinger(phi, soln$solution %*% t(beta_mx)) |> 
        diag()

    ## Topic-doc distributions
    gamma_df = tidy(fitted, k, 'gamma', rotation = soln$solution)
    
    theta_accuracy = hellinger(rename(theta_df, gamma = prob), 'doc', 
                            topics2 = gamma_df, id2 = 'doc', 
                            df = TRUE) |> 
        filter(doc_x == doc_y) |> 
        pull(dist)
    
    tibble(phi = mean(phi_accuracy), 
           phi_vec = list(phi_accuracy), 
           theta = mean(theta_accuracy), 
           theta_vec = list(theta_accuracy))
}
