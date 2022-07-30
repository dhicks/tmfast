#' Information gain (uniform distribution)
#'
#' Calculates \eqn{\log_2 n \times \delta H}, the log total occurrence times information gain (relative to the uniform distribution) for each term. I prefer this for vocabulary selection over methods such as TF-IDF.
#' @param dataf Tidy document-term matrix
#' @param doc_col Column of `dataf` with document IDs
#' @param term_col Column of `dataf` with terms
#' @param count_col Column of `dataf` with document-term counts
#' @return Dataframe with columns
#'
#'     - `{{ term col }}`, term
#'     - `dH`, information gain relative to uniform distribution over documents
#'     - `n`, total count of term occurrence
#'     - `ndH`, \eqn{\log_2 n \times \delta H}
#' @examples
#' library(tidyverse)
#' library(tidytext)
#' library(janeaustenr)
#' austen_df = austen_books() |>
#'     unnest_tokens(term, text, token = 'words') |>
#'     mutate(author = 'Jane Austen') |>
#'     count(author, book, term)
#' ndH(austen_df, book, term, n)
#' @export
ndH = function(dataf, doc_col, term_col, count_col) {
    n_docs = dataf |>
        dplyr::pull({{ doc_col }}) |>
        dplyr::n_distinct()

    dataf |>
        dplyr::group_by({{ term_col }}) |>
        dplyr::mutate(p = {{ count_col }} / sum({{ count_col }}),
                      H_term = -p*log2(p)) |>
        dplyr::summarize(H = sum(H_term),
                         dH = log2(n_docs) - H,
                         n = sum({{ count_col }})) |>
        dplyr::ungroup() |>
        dplyr::mutate(ndH = log2(n)*dH) |>
        dplyr::arrange(desc(ndH))
}

ndH.ArrowObject = function(dataset, doc_col, term_col, count_col) {
    n_docs = dataset |>
        dplyr::pull({{ doc_col }}) |>
        dplyr::n_distinct()

    totals = dataset |>
        dplyr::group_by({{ term_col }}) |>
        dplyr::summarize(n_tot = sum(n))

    result = dataset |>
        dplyr::left_join(totals,
                         by = rlang::as_label(enquo(term_col))) |>
        dplyr::mutate(p = {{ count_col }} / n_tot,
               H_term = -p * log2(p)) |>
        dplyr::group_by({{ term_col }}) |>
        dplyr::summarize(H = sum(H_term),
                  {{ count_col }} := sum({{ count_col }})) |>
        dplyr::ungroup() |>
        dplyr::mutate(dH = log2(n_docs) - H,
               ndH = log2(n) * dH) |>
        dplyr::arrange(desc(ndH))
    return(result)
}

#' Information gain (length-proportional distribution)
#'
#' An alternative to `ndH()` that uses information gain relative to a distribution of documents that is proportional to length.  With the uniform distribution and dramatic differences in document lengths (eg, over a few orders of magnitude), high-ndH terms tend to be distinctive terms from very long documents.  With the length-proportional distribution, high information-gain terms are more likely to come from shorter documents. Informal testing suggests this approach performs better than the `ndH()` uniform distribution when documents have widely varying lengths, eg, over a few orders of magnitude.
#' @param dataf Tidy document-term matrix
#' @param doc_col Column of `dataf` with document IDs
#' @param term_col Column of `dataf` with terms
#' @param count_col Column of `dataf` with document-term counts
#' @return Dataframe with columns
#'
#'     - `{{ term col }}`, term
#'     - `n`, total count of term occurrence
#'     - `dR`, information gain relative to length-proportional distribution over documents
#'     - `ndR`, \eqn{\log_2 n \times \delta R}
#' @examples
#' library(tidyverse)
#' library(tidytext)
#' library(janeaustenr)
#' austen_df = austen_books() |>
#'     unnest_tokens(term, text, token = 'words') |>
#'     mutate(author = 'Jane Austen') |>
#'     count(author, book, term)
#' ndR(austen_df, book, term, n)
#' @export
ndR = function(dataf, doc_col, term_col, count_col) {
    ## Document lengths
    ## len: Length of doc_j
    ## r: Probability of drawing doc_j, len / sum(len) across all docs
    alpha = dataf |>
        pull({{ count_col }}) |>
        sum()
    r_df = dataf |>
        dplyr::group_by({{ doc_col }}) |>
        dplyr::summarize(len = sum({{ count_col }})) |>
        dplyr::ungroup() |>
        dplyr::mutate(r = len/alpha)

    ## Termwise total occurrences
    totals = dataf |>
        dplyr::group_by({{ term_col }}) |>
        dplyr::summarize(n_tot = sum(n))

    ## Conditional entropy for each term, and KL divergence wrt r/R
    result = dataf |>
        dplyr::left_join(r_df, by = rlang::as_name(enquo(doc_col))) |>
        dplyr::left_join(totals, by = rlang::as_name(enquo(term_col))) |>
        dplyr::mutate(p = n / n_tot,
                      dR_term = p * log2(p / r)) |>
        dplyr::group_by({{ term_col }}) |>
        dplyr::summarize(n = sum(n),
                         dR = sum(dR_term)) |>
        dplyr::ungroup() |>
        dplyr::mutate(ndR = log2(n)*dR) |>
        dplyr::arrange(desc(ndR))
    return(result)
}

