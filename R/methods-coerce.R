#' Coerce to `tibble` Class Object
#'
#' Coerce an object to a `tibble` `data.frame`. Tibbles don't support rowname
#' assignemnt, so here we are ensuring they are kept by converting the rownames
#' to a standard `data.frame` column named `rowname` upon coercion. This helps
#' avoid downstream unexpected data loss when using the dplyr chain of single
#' table verbs, such as [dplyr::arrange()], [dplyr::filter()], or
#' [dplyr::mutate()].
#'
#' @name coerceToTibble
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @param from Class for which the coerce method will perform coercion.
#'
#' @seealso `help(topic = "coerce", package = "methods")`.
NULL



# Constructors =================================================================
.as.tibble <- function(from) {  # nolint
    assert_has_dims(from)
    assert_has_colnames(from)
    from <- as.data.frame(from)
    if (tibble::has_rownames(from)) {
        from <- rownames_to_column(from)
    }
    tibble::as_tibble(from)
}



# Methods ======================================================================
setAs(from = "matrix", to = "tbl_df", def = .as.tibble)
setAs(from = "data.frame", to = "tbl_df", def = .as.tibble)
setAs(from = "DataFrame", to = "tbl_df", def = .as.tibble)

setAs(from = "matrix", to = "tibble", def = .as.tibble)
setAs(from = "data.frame", to = "tibble", def = .as.tibble)
setAs(from = "DataFrame", to = "tibble", def = .as.tibble)
