#' Coerce to [tibble] Class Object
#'
#' Coerce an object to a [tibble] [data.frame]. Tibbles don't support rowname
#' assignemnt, so here we are ensuring they are kept by converting the rownames
#' to a standard [data.frame] column named `rowname` upon coercion. This helps
#' avoid downstream unexpected data loss when using the dplyr chain of single
#' table verbs, such as [dplyr::arrange()], [dplyr::filter()], or
#' [dplyr::mutate()].
#'
#' @rdname coerceToTibble
#' @name coerceToTibble
#' @keywords internal
#'
#' @param from Class for which the coerce method will perform coercion.
#'
#' @seealso `help(topic = "coerce", package = "methods")`.
NULL



# Constructors =================================================================
#' @importFrom tibble as_tibble rownames_to_column
.as.tibble <- function(from) {  # nolint
    assert_has_dims(from)
    from <- as.data.frame(from)
    # Don't use `assertive::has_rownames()` here
    if (tibble::has_rownames(from)) {
        from <- rownames_to_column(from)
    }
    as_tibble(from)
}



# Methods ======================================================================
# We may want to manually define the classes here in a stricter manner in
# the future (e.g. `DataFrame`, `data.frame`, `Matrix`, `matrix`, `dgCMatrix`).
setAs("ANY", "tbl_df", .as.tibble)
setAs("ANY", "tibble", .as.tibble)
