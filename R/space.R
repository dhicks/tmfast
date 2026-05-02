#' Discursive space using t-SNE
#'
#' 2-dimensional "discursive space" representation of relationships between
#' documents using Hellinger distances and t-SNE.
#' @param x Object to dispatch on
#' @param ... Passed to methods
#' @param perplexity Perplexity parameter for t-SNE. By default, minimum of 30
#'   and `floor((length(doc_ids) - 1)/3) - 1`.
#' @param df Return a dataframe with columns `document`, `x`, and `y` (default)
#'   or the raw output of `Rtsne`.
#' @details Algorithm checks distances to 3*perplexity nearest neighbors. Rtsne
#'   loses rownames (document IDs); these are either extracted from the `tmfast`
#'   object or passed separately for an `STM` object. Use `set.seed()` before
#'   calling for reproducibility.
#' @return See `df`
#' @examples
#' \donttest{
#' set.seed(42)
#' theta = rdirichlet(50, 1, k = 3)
#' phi   = rdirichlet(3, 0.1, k = 30)
#' corpus = draw_corpus(rep(50L, 50), theta, phi)
#' fitted = tmfast(corpus, n = 3)
#' tsne(fitted, k = 3, df = TRUE)
#' }
#' @export
tsne = function(x, ...) {
      UseMethod('tsne')
}

#' @describeIn tsne Method for tidied gamma dataframes
#' @param x Tidied document-topic gamma dataframe, as returned by
#'   `tidy(model, matrix = 'gamma')`
#' @param doc_ids Vector of document IDs, in the same order as rows in `x`
#' @export
tsne.data.frame = function(
      x,
      doc_ids,
      perplexity = NULL,
      df = TRUE,
      ...
) {
      rlang::check_dots_empty()
      if (is.null(perplexity)) {
            ndocs = length(doc_ids)
            perplexity = min(30, floor((ndocs - 1) / 3) - 1)
      }
      if (!requireNamespace("Rtsne", quietly = TRUE)) {
            stop("Package 'Rtsne' is required for tsne(). ",
                 "Install it with: install.packages('Rtsne')")
      }
      fitted_tsne = x |>
            hellinger(id1 = 'document', prob1 = 'gamma') |>
            Rtsne::Rtsne(perplexity = perplexity, is_distance = TRUE)
      if (!df) {
            return(fitted_tsne)
      }
      fitted_tsne$Y |>
            magrittr::set_rownames(doc_ids) |>
            tibble::as_tibble(rownames = 'document', .name_repair = \(x) {
                  (c('x', 'y'))
            })
}
#' @describeIn tsne Method for fitted `tmfast` objects
#' @param x Fitted topic model (`tmfast` or `STM`)
#' @param k Number of topics
#' @export
tsne.tmfast = function(x, k, perplexity = NULL, df = TRUE, ...) {
      rlang::check_dots_empty()
      doc_ids = rownames(scores(x, k))
      gamma_df = tidy(x, k, matrix = 'gamma')
      tsne.data.frame(gamma_df, doc_ids, perplexity, df)
}
#' @describeIn tsne Method for fitted `STM` objects
#' @export
tsne.STM = function(x, doc_ids, perplexity = NULL, df = TRUE, ...) {
      rlang::check_dots_empty()
      gamma_df = tidy(x, matrix = 'gamma')
      tsne.data.frame(gamma_df, doc_ids, perplexity, df)
}


#' Discursive space using UMAP
#'
#' 2-dimensional "discursive space" representation of relationships between
#' documents using Hellinger distances and UMAP.
#' @param x Object to dispatch on
#' @param ... Passed to methods
#' @param df Return a tibble with columns `document`, `x`, and `y` (default) or
#'   the raw `umap` object.
#' @return Tibble with columns `document`, `x`, `y` when `df = TRUE`; otherwise
#'   an object of class `umap` with components `layout`, `knn`, and `config`.
#' @export
umap = function(x, ...) {
      UseMethod('umap')
}

#' @describeIn umap Method for distance matrices
#' @param x Square distance matrix (documents x documents)
#' @param include_data Return the distance matrix inside the umap object?
#'   Default `FALSE` to save memory.
#' @examples
#' gamma = rdirichlet(26, 1, 5)
#' rownames(gamma) = letters
#' h_gamma = hellinger(gamma)
#' umap(h_gamma, df = TRUE)
#' @export
umap.matrix = function(x, include_data = FALSE, df = TRUE, ...) {
      if (!requireNamespace("umap", quietly = TRUE)) {
            stop("Package 'umap' is required for umap(). ",
                 "Install it with: install.packages('umap')")
      }
      embedding = umap::umap(x, input = 'dist', ...)
      if (!include_data) {
            embedding$data = NULL
      }
      rownames(embedding$layout) = rownames(x)
      if (df) {
            embedding = embedding$layout |>
                  tibble::as_tibble(rownames = 'document', .name_repair = \(x) {
                        (c('x', 'y'))
                  })
      }
      return(embedding)
}

#' @describeIn umap Method for fitted `tmfast` objects
#' @param x `tmfast` object
#' @param k Number of topics
#' @examples
#' \donttest{
#' set.seed(42)
#' theta = rdirichlet(30, 1, k = 3)
#' phi   = rdirichlet(3, 0.1, k = 30)
#' corpus = draw_corpus(rep(50L, 30), theta, phi)
#' fitted = tmfast(corpus, n = 3)
#' umap(fitted, 3)
#' }
#' @export
umap.tmfast = function(x, k, ...) {
      distances = tidy(x, k, matrix = 'gamma') |>
            hellinger(prob1 = 'gamma', df = FALSE)
      embedding = umap.matrix(distances, ...)
      return(embedding)
}

#' @describeIn umap Method for fitted `STM` objects
#' @param x Fitted `STM` object
#' @param doc_ids Character vector of document IDs
#' @export
umap.STM = function(x, doc_ids, ...) {
      k = ncol(x$theta)
      distances = tidy(x, matrix = 'gamma') |>
            hellinger(prob1 = 'gamma', df = FALSE)
      rownames(distances) = doc_ids
      embedding = umap.matrix(distances, ...)
      return(embedding)
}
