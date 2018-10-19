#' @name as
#' @aliases coerce
#' @importFrom methods coerce
#' @inherit methods::as
#' @inheritParams methods::coerce
#' @exportMethod coerce
#'
#' @section list:
#'
#' It is often useful to coerce an S4 object to a flat `list` for archival
#' storage. Here we are providing the [coerceS4ToList()] function, which
#' consistently coerces the slots in any S4 to a standard `list`. Additionally,
#' here we have improved support for `SummarizedExperiment` to `list` coercion,
#' returning the slots as a `list`.
#'
#' @section tibble:
#'
#' Coerce an object to a [tibble][] (`tbl_df`) data frame using either S3 or S4
#' methods:
#'
#' - S3: `as_tibble(x)` (or `as_tibble()` alias).
#' - S4: `as(object, Class = "tbl_df")`.
#'
#' Tibbles don't support row name assignment, so here we are ensuring they are
#' kept by moving them to a column named `rowname` by default. This helps avoid
#' downstream unexpected data loss when using the dplyr chain of single table
#' verbs, such as [dplyr::arrange()], [dplyr::filter()], or [dplyr::mutate()].
#'
#' This behavior can be overriden in the S3 method by setting `rowname = NULL`
#' instead, which is the current default in the tibble package. The S4 coercion
#' method doesn't support arguments, and therefore always attempts to move
#' rownames automatically, if defined.
#'
#' Conversely, when coercing a `tibble` back to an S4 `DataFrame`, our
#' `as(tbl_df, Class = "DataFrame")` method looks for the `rowname` column and
#' will attempt to move it back to [base::rownames()] automatically, unless
#' there are duplicates present.
#'
#' [tibble]: https://tibble.tidyverse.org/
#'
#' @inheritParams general
#'
#' @return Object of new class.
#'
#' @seealso
#' - [methods::as()].
#' - [methods::canCoerce()].
#' - [methods::showMethods()].
#' - [utils::methods()].
#'
#' @examples
#' data(rse_small)
#'
#' ## SummarizedExperiment to list ====
#' x <- as(rse_small, Class = "list")
#' class(x)
#' names(x)
#'
#' ## DataFrame to tbl_df ====
#' data <- SummarizedExperiment::colData(rse_small)
#' print(data)
#' x <- as(data, "tbl_df")
#' print(x)
NULL



# data.frame ===================================================================
#' @rdname as
#' @inheritParams BiocGenerics::as.data.frame
#' @export
setMethod(
    f = "as.data.frame",
    signature = signature("sparseMatrix"),
    definition = function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }
)



#' @rdname as
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)



#' @rdname as
#' @name coerce,sparseMatrix,DataFrame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



# DataFrame ====================================================================
as.DataFrame <-  # nolint
    function(x) {
        UseMethod("as.DataFrame")
    }



as.DataFrame.default <-  # nolint
    function(x) {
        to <- as.data.frame(x, stringsAsFactors = FALSE)
        to <- as(to, "DataFrame")
        rownames <- as.character(to[["rowname"]])
        if (
            has_length(rownames) &&
            !any(duplicated(rownames))
        ) {
            rownames(to) <- rownames
            to[["rowname"]] <- NULL
        }
        to
    }



#' @rdname as
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



# list =========================================================================
#' @rdname as
#' @export
coerceS4ToList <- function(from) {
    stopifnot(isS4(from))
    to <- lapply(slotNames(from), function(slot) {
        if (.hasSlot(from, slot)) {
            slot(from, slot)
        } else {
            NULL  # nocov
        }
    })
    names(to) <- slotNames(from)
    to
}



#' @rdname as
#' @export
setMethod(
    f = "as.list",
    signature = signature("SummarizedExperiment"),
    definition = function(x) {
        coerceS4ToList(x)
    }
)



#' @rdname as
#' @name coerce,SummarizedExperiment,list-method
setAs(
    from = "SummarizedExperiment",
    to = "list",
    def = coerceS4ToList
)



# SummarizedExperiment =========================================================
as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



as.SummarizedExperiment.default <-  # nolint
    function(x) {
    assert_is_all_of(x, "SummarizedExperiment")
    if (is(x, "RangedSummarizedExperiment")) {
        x <- as(x, "RangedSummarizedExperiment")
    }
    x <- as(x, "SummarizedExperiment")
    x
}



# tibble =======================================================================
#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble



#' @method as_tibble DataFrame
#' @export
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames = "rowname") {
        # Check for valid columns (atomic, list).
        valid <- vapply(
            X = x,
            FUN = function(x) {
                is.atomic(x) || is.list(x)
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        )
        # Error if S4 columns are nested.
        if (!all(valid)) {
            invalid <- names(valid[!valid])
            stop(paste0(
                "tibble supports atomic and list columns.\n",
                "Invalid columns: ", toString(invalid)
            ), call. = FALSE)
        }
        # Coerce to standard data frame.
        x <- as(x, "data.frame")
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        do.call(
            what = as_tibble,
            args = list(
                x = x,
                ...,
                rownames = rownames
            )
        )
    }



#' @rdname as
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)



# The default handling from data.frame isn't clean, so add this.
# Default method will warn: `Arguments in '...' ignored`.
#' @method as_tibble GRanges
#' @export
as_tibble.GRanges <-  # nolint
    function(x, ..., rownames = "rowname") {
        names <- names(x)
        x <- as(x, "data.frame")
        rownames(x) <- names
        if (!hasRownames(x)) {
            rownames <- NULL
        }
        do.call(
            what = as_tibble,
            args = list(
                x = x,
                ...,
                rownames = rownames
            )
        )
    }



#' @rdname as
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)
