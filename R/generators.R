#' Functions to generate document-term matrices

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
draw_a_word = function(theta, .phi = phi) {
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
draw_words = function(Ni, theta, .phi = phi) {
    word = map_int(1:Ni, ~draw_a_word(theta, phi))
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
            ~draw_words(N[.x], theta[.x,], phi[.x,]), 
            .id = 'doc') |> 
        mutate(doc = as.integer(doc))
}
