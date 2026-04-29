#' Hellinger distances
#'
#' @param topics1 Object to dispatch on
#' @param ... Passed to methods
#' @export
hellinger = function(topics1, ...) {
    UseMethod("hellinger")
}

#' @importFrom Matrix t
#' @importFrom Matrix crossprod
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix which
NULL

#' Hellinger distance for matrices
#'
#' Calculates Hellinger distance for each pair of rows in the given matrix, or each combination of rows from the two matrices
#' @param topics1 First matrix, \eqn{n_1 \times k}
#' @param topics2 Optional second matrix, \eqn{n_2 \times k}
#' @param ... Not used; required for S3 method compatibility
#' @returns Matrix of size \eqn{n_1 \times n_1} or \eqn{n_1 \times n_2}
#' @export
#' @examples
#' set.seed(2022-06-09)
#' topics1 = rdirichlet(3, rep(5, 5))
#' topics2 = rdirichlet(3, rep(5, 5))
#' hellinger(topics1)
#' hellinger(topics1, topics2)
hellinger.Matrix = function(topics1, topics2 = NULL, ...) {
    rlang::check_dots_empty()
    if (is.null(topics2)) {
        crossed = 1 - tcrossprod(sqrt(topics1))
    } else {
        crossed = 1 - tcrossprod(sqrt(topics1), sqrt(topics2))
    }
    crossed[which(crossed < 0)] = 0
    crossed = sqrt(crossed)
    return(crossed)
}
#' @export
hellinger.matrix = function(...) hellinger.Matrix(...)

#' Convert a long dataframe to a wide (sparse) matrix
#'
#' An alias for `tidytext::cast_sparse`
#' @param data Dataframe
#' @param row Column name to use as row names, as string or symbol
#' @param column Column name to use as column names, as string or symbol
#' @param value Column name to use as matrix values, as string or symbol
#' @param ... Other arguments, passed to `Matrix::sparseMatrix`
#' @param sparse Should the matrix be a `Matrix` sparse matrix?
#' @returns A matrix or sparse Matrix object, with one row for each unique value in the row column, one column for each unique value in the column column, and with as many non-zero values as there are rows in data.
#' @examples
#' data.frame(id = c(1, 1, 2, 2) + 4,
#'            cols = c('a', 'b', 'a', 'b'),
#'            vals = 1:4) |>
#'     build_matrix(row = id, column = 'cols', value = vals)
#' @export
build_matrix = function(data, row, column, value, ..., sparse = TRUE) {
    if (sparse) {
        tidytext::cast_sparse(data, {{row}}, {{column}}, {{value}}, ...)
    } else {
        data |>
            dplyr::select({{ row }}, {{ column }}, {{ value }}) |>
            tidyr::pivot_wider(id_cols = {{ row }},
                               names_from = {{ column }},
                               values_from = {{ value }},
                               values_fill = 0) |>
            tibble::column_to_rownames(var = rlang::as_name(rlang::enquo(row))) |>
            as.matrix()
    }
}

#' Hellinger distance for dataframes
#'
#' Hellinger distances, either pairwise within a single tidied topic model dataframe or between two tidied topic model dataframes
#' @param topics1 First tidied topic model dataframe
#' @param id1 Unit identifier column in `topics1` (DOIs, auids, ORU name, etc.)
#' @param cat1 Category identifier column in `topics1` (topics)
#' @param prob1 Probability value column in `topics1` (gamma)
#' @param topics2 Optional second tidied topic model dataframe
#' @param id2 Unit identifier column in `topics2`
#' @param cat2 Category identifier column in `topics2`
#' @param prob2 Probability value column in `topics2`
#' @param df Should the function return the matrix of Hellinger distances (default) or a tidy dataframe?
#' @param ... Not used; required for S3 method compatibility
#' @return matrix or tidy dataframe (default) of Hellinger distances
#' @export
#' @examples
#' set.seed(2022-06-09)
#' topics1 = rdirichlet(3, rep(5, 5)) |>
#'     tibble::as_tibble(rownames = 'doc_id',
#'                       .name_repair = tmfast:::make_colnames) |>
#'     dplyr::mutate(doc_id = stringr::str_c('doc_', doc_id)) |>
#'     tidyr::pivot_longer(tidyselect::starts_with('V'),
#'                         names_to = 'topic',
#'                         values_to = 'gamma')
#' topics2 = rdirichlet(3, rep(5, 5)) |>
#'     tibble::as_tibble(rownames = 'doc_id',
#'                       .name_repair = tmfast:::make_colnames) |>
#'     dplyr::mutate(doc_id = stringr::str_c('doc_', as.integer(doc_id) + 5)) |>
#'     tidyr::pivot_longer(tidyselect::starts_with('V'),
#'                         names_to = 'topic',
#'                         values_to = 'gamma')
#' hellinger(topics1, doc_id, prob1 = 'gamma', df = TRUE)
#' hellinger(topics1, doc_id, prob1 = 'gamma',
#'           topics2 = topics2, id2 = doc_id, prob2 = 'gamma')
hellinger.data.frame = function(topics1,
                                id1 = 'document',
                                cat1 = 'topic',
                                prob1 = 'prob',
                                topics2 = NULL,
                                id2 = 'document',
                                cat2 = 'topic',
                                prob2 = 'prob',
                                df = FALSE,
                                ...) {
    rlang::check_dots_empty()
    id1 = rlang::enquo(id1)
    matrix1 = build_matrix(topics1, {{id1}}, {{cat1}}, {{prob1}},
                           sparse = FALSE)
    id2 = rlang::enquo(id2)
    if (is.null(topics2)) {
        hellinger_matrix = hellinger(matrix1)
    } else {
        matrix2 = build_matrix(topics2, {{id2}}, {{cat2}}, {{prob2}},
                               sparse = FALSE)
        hellinger_matrix = hellinger(matrix1, matrix2)
    }

    if (!df) {
        return(hellinger_matrix)
    }

    id1 = rlang::as_name(id1)
    id2 = rlang::as_name(id2)
    if (assertthat::are_equal(id1, id2)) {
        id1 = stringr::str_c(id1, '_x')
        id2 = stringr::str_c(id2, '_y')
    }
    hellinger_matrix |>
        as.matrix() |>
        tibble::as_tibble(rownames = id1) |>
        tidyr::pivot_longer(-tidyselect::all_of(id1),
                            names_to = id2,
                            values_to = 'dist')
}




