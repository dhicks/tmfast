#' Hellinger distances
#'
#' @export
hellinger = function(x, ...) {
    UseMethod("hellinger")
}

#' @importFrom Matrix t
NULL

#' Hellinger distance for matrices
#'
#' Calculates Hellinger distance for each pair of rows in the given matrix, or each combination of rows from the two matrices
#' @param mx1 First matrix, \eqn{n_1 \times k}
#' @param mx2 Optional second matrix, \eqn{n_2 \times k}
#' @returns Matrix of size \eqn{n_1 \times n_1} or \eqn{n_1 \times n_2}
#' @export
#' @examples
#' set.seed(2022-06-09)
#' mx1 = rdirichlet(3, rep(5, 5))
#' mx2 = rdirichlet(3, rep(5, 5))
#' hellinger(mx1)
#' hellinger(mx1, mx2)
hellinger.Matrix = function(mx1, mx2 = NULL) {
    if (is.null(mx2)) {
        mx2 = t(mx1)
    } else {
        mx2 = t(mx2)
    }
    assertthat::assert_that(assertthat::are_equal(ncol(mx1), nrow(mx2)),
                msg = 'Matrices must have same number of columns')

    mx1.2 = sqrt(mx1) %*% sqrt(mx2)
    return(sqrt(pmax(1 - mx1.2, 0)))
    # return(sqrt(round(1 - mx1.2, 8)))
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
#' @returns A sparse Matrix object, with one row for each unique value in the row column, one column for each unique value in the column column, and with as many non-zero values as there are rows in data.
#' @examples
#' data.frame(id = c(1, 1, 2, 2) + 4,
#'            cols = c('a', 'b', 'a', 'b'),
#'            vals = 1:4) |>
#'     build_matrix(row = id, column = 'cols', value = vals)
#' @export
build_matrix = function(data, row, column, value, ...) {
    tidytext::cast_sparse(data, {{row}}, {{column}}, {{value}}, ...)
}

#' Hellinger distance
#'
#' Hellinger distances, either pairwise within a single tidied topic model dataframe or between two tidied topic model dataframes
#' @param {topicsdf1,topicsdf2} Tidied topic model dataframes
#' @param {id1,id2} Unit identifiers (DOIs, auids, ORU name, etc.)
#' @param {cat1,cat2} Category identifiers (topics)
#' @param {prob1,prob2} Probability values (gamma)
#' @param df Should the function return the matrix of Hellinger distances (default) or a tidy dataframe?
#' @return matrix or tidy dataframe (default) of Hellinger distances
#' @export
#' @examples
#' set.seed(2022-06-09)
#' topics1 = rdirichlet(3, rep(5, 5)) |>
#'     tibble::as_tibble(rownames = 'doc_id', .name_repair = make_colnames) |>
#'     dplyr::mutate(doc_id = stringr::str_c('doc_', doc_id)) |>
#'     tidyr::pivot_longer(tidyselect::starts_with('V'),
#'                         names_to = 'topic',
#'                         values_to = 'gamma')
#' topics2 = rdirichlet(3, rep(5, 5)) |>
#'     tibble::as_tibble(rownames = 'doc_id', .name_repair = make_colnames) |>
#'     dplyr::mutate(doc_id = stringr::str_c('doc_', as.integer(doc_id) + 5)) |>
#'     tidyr::pivot_longer(tidyselect::starts_with('V'),
#'                         names_to = 'topic',
#'                         values_to = 'gamma')
#' hellinger(topics1, doc_id, prob1 = 'gamma', df = TRUE)
#' hellinger(topics1, doc_id, prob1 = 'gamma',
#'           topicsdf2 = topics2, id2 = doc_id, prob2 = 'gamma')
hellinger.data.frame = function(topicsdf1, id1, cat1 = 'topic', prob1 = 'prob',
                     topicsdf2 = NULL, id2 = NULL, cat2 = 'topic', prob2 = 'prob',
                     df = FALSE) {
    id1 = rlang::enquo(id1)
    matrix1 = build_matrix(topicsdf1, {{id1}}, {{cat1}}, {{prob1}})
    id2 = rlang::enquo(id2)
    if (rlang::quo_is_null(id2)) {
        id2 = {{id1}}
    }
    if (is.null(topicsdf2)) {
        matrix2 = matrix1
    } else {
        matrix2 = build_matrix(topicsdf2, {{id2}}, {{cat2}}, {{prob2}})
    }

    hellinger_matrix = hellinger(matrix1, matrix2)
    # hellinger_matrix = tidyr::replace_na(hellinger_matrix, 0.0)

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
        tidyr::pivot_longer(-one_of(id1),
                     names_to = id2,
                     values_to = 'dist')
}


