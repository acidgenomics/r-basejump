#' Methods for Coercing an Object to a Class
#'
#' @name coerce
#' @aliases as
#' @author Michael Steinbaugh
#' @exportMethod coerce
#'
#' @section tibble:
#' Coerce an object to a `tibble` (`tbl_df`). Tibbles don't support rowname
#' assignemnt, so here we are ensuring they are kept by moving them to a column
#' named `rowname` upon coercion. This helps avoid downstream unexpected data
#' loss when using the dplyr chain of single table verbs, such as
#' [dplyr::arrange()], [dplyr::filter()], or [dplyr::mutate()].
#'
#' @return Object of new class.
#'
#' @seealso
#' - [methods::as()].
#' - [methods::coerce()].
#' - `help(topic = "tibble", package = "tibble")`
#' - `help(topic = "coerce", package = "methods")`.
#'
#' @examples
#' # data.frame ====
#' # Automatically move rownames to `rowname` column
#' as(datasets::mtcars, "tibble") %>% glimpse()
#'
#' # tibble ====
#' # Return unmodified
#' as(ggplot2::mpg, "tibble") %>% glimpse()
NULL



# list =========================================================================
#' @rdname coerce
#' @name coerce,SummarizedExperiment,list-method
setAs(
    from = "SummarizedExperiment",
    to = "list",
    function(from) {
        flatFiles(from)
    }
)



# tibble =======================================================================
# Help avoid dropping rownames during tidyverse function calls.
.as_tibble <- function(from) {  # nolint
    if (is_tibble(from)) {
        return(from)  # nocov
    }
    from <- as.data.frame(from)
    assert_has_colnames(from)
    if (has_rownames(from)) {
        from <- rownames_to_column(from)
    }
    as_tibble(from)
}

#' @rdname coerce
#' @name coerce,matrix,tbl_df-method
setAs(from = "matrix", to = "tbl_df", def = .as_tibble)

#' @rdname coerce
#' @name coerce,data.frame,tbl_df-method
setAs(from = "data.frame", to = "tbl_df", def = .as_tibble)

#' @rdname coerce
#' @name coerce,DataFrame,tbl_df-method
setAs(from = "DataFrame", to = "tbl_df", def = .as_tibble)

#' @rdname coerce
#' @name coerce,GRanges,tbl_df-method
setAs(from = "GRanges", to = "tbl_df", def = .as_tibble)
