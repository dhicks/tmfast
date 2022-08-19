#' Discursive space using t-SNE
#' @export
tsne = function(x, ...) {
    UseMethod('tsne')
}

#' Discursive space using t-SNE
#'
#' 2-dimensional "discursive space" representation of relationships between documents using Hellinger distances and t-SNE
#' @param tm A fitted topic model
#' @param k  Number of topics (required for `tmfast` objects)
#' @param doc_ids Vector of document IDs (required for `STM` objects)
#' @param perplexity Perplexity parameter for t-SNE. By default, minimum of 30 and `floor((ndocs - 1)/3) - 1`.
#' @param df Return a dataframe with columns `document`, `x`, and `y` (default) or the output of `Rtsne`.
#' @details Algorithm checks distances to 3*perplexity nearest neighbors.  Rtsne loses rownames (document IDs); these are either extract from the `tmfast` object or passed separately for a `STM`object.  The default method (not exported) takes a tidied gamma (document-topic-gamma) matrix.  Use `set.seed()` before calling this function for reproducibility.
#' @return See `df`
#' @examples
#' ## From the real books vignette
#' set.seed(42)
#' tsne(fitted_tmf, k = 4, df = TRUE) |>
#'     left_join(meta, by = c('document' = 'book')) |>
#'     ggplot(aes(x, y, color = author)) +
#'     geom_point()
tsne.data.frame = function(gamma_df, k, doc_ids,
                        perplexity = NULL, df = TRUE) {
    if (is.null(perplexity)) {
        ndocs = length(doc_ids)
        perplexity = min(30, floor((ndocs - 1)/3) - 1)
    }
    fitted_tsne = gamma_df |>
        hellinger(id1 = document, prob1 = gamma) |>
        Rtsne::Rtsne(perplexity = perplexity, is_distance = TRUE)
    if (!df) {
        return(fitted_tsne)
    }
    fitted_tsne$Y |>
        magrittr::set_rownames(doc_ids) |>
        tibble::as_tibble(rownames = 'document',
                          .name_repair = \(x)(c('x', 'y')))
}
#' @export
tsne.tmfast = function(tm, k, perplexity = NULL, df = TRUE) {
    doc_ids = rownames(scores(tm, k))
    gamma_df = tidy(tm, k, matrix = 'gamma')
    tsne.data.frame(gamma_df, k, doc_ids, perplexity, df)
}
#' @export
tsne.STM = function(tm, doc_ids, perplexity = NULL, df = TRUE) {
    k = ncol(tm$theta)
    gamma_df = tidy(tm, matrix = 'gamma')
    tsne.data.frame(gamma_df, k, doc_ids, perplexity, df)
}


#' Discursive space using UMAP
#'
#' 2-dimensional "discursive space" representation of relationships between documents using Hellinger distances and UMAP
#' @export
umap = function(x, ...) {
    UseMethod('umap')
}
#' Discursive space with UMAP given a distance matrix
#'
#' Construct a 2-dimensional "discursive space" embedding given a distance matrix.
#' @param dist_mx Distance matrix
#' @param include_data By default, to save space the data (distance matrix) is not returned
#' @param df Return a tibble with columns `document`, `x`, and `y` (default) or the output of `umap`.
#' @param ... Other parameters passed to `umap::umap()`
#' @return Object of class `umap`, with components `layout` (coordinates of items), `knn` (k-nearest neighbors matrices), `config` (UMAP configuration) *or* tidied dataframe, per argument `df`
#' @examples
#' gamma = rdirichlet(26, 1, 5)
#' rownames(gamma) = letters
#' h_gamma = hellinger(gamma)
#' embedded = umap(h_gamma, df = TRUE, verbose = TRUE)
#' @export
umap.matrix = function(dist_mx, include_data = FALSE, df = TRUE, ...) {
    embedding = umap::umap(dist_mx, input = 'dist', ...)
    if (!include_data) {
        embedding$data = NULL
    }
    rownames(embedding$layout) = rownames(dist_mx)
    if (df) {
        embedding = embedding$layout |>
            tibble::as_tibble(rownames = 'document',
                              .name_repair = \(x)(c('x', 'y')))
    }
    return(embedding)
}

#' Discursive space with UMAP for tmfast topic models
#'
#' Construct a 2-dimensional "discursive space" embedding given a `tmfast` topic model
#' @param model `tmfast` object
#' @param k Number of topics
#' @param ... Other arguments, passed to `umap.matrix()`
#' @return `umap` object or tidied dataframe; see `umap.matrix()` argument `df`
#' @examples
#' umap(fitted, 10, verbose = TRUE)
#' @export
umap.tmfast = function(model, k, ...) {
    distances = tidy(model, k, matrix = 'gamma') |>
        hellinger(prob1 = 'gamma', df = FALSE)
    embedding = umap.matrix(distances, ...)
    return(embedding)
}

#' Discursive space with UMAP for structural topic models
#'
#' @param tm `STM` object
#' @param doc_ids Character vector of document IDs
#' @param ... Other arguments, passed to `umap.matrix()`
#' @return `umap` object or tidied dataframe; see `umap.matrix()` argument `df`
#' @export
umap.STM = function(tm, doc_ids, ...) {
    k = ncol(tm$theta)
    distances = tidy(tm, matrix = 'gamma') |>
        hellinger(prob1 = 'gamma', df = FALSE)
    rownames(distances) = doc_ids
    embedding = umap.matrix(distances, ...)
}
