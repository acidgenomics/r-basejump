#' Methods for Coercing an Object to a Class
#'
#' @section tibble:
#'
#' Coerce an object to a `tibble` (`tbl_df`) data frame using either S3 or S4
#' methods:
#'
#' - S3: `as_tibble(x)` (or `as.tibble()` alias).
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
#' will attempt to move it back to [rownames()] automatically, unless there are
#' duplicates present.
#'
#' @section list:
#'
#' It is often useful to coerce an S4 object to a flat `list` for archival
#' storage. Here we are providing the [coerceS4ToList()] function, which
#' consistently coerces the slots in any S4 to a standard `list`. Additionally,
#' here we have improved support for `SummarizedExperiment` to `list` coercion,
#' returning the slots as a `list`.
#'
#' @name coerce
#' @aliases as
#' @importFrom methods coerce
#' @exportMethod coerce
#'
#' @inheritParams general
#' @inheritParams methods::coerce
#'
#' @return Object of new class.
#'
#' @seealso
#' - [methods::as()].
#' - [methods::canCoerce()].
#' - [methods::showMethods()].
#' - [utils::methods()].
#' - [tibble::as_tibble()].
#' - [as.list()].
#'
#' @examples
#' data(rse_small)
#'
#' ## DataFrame to tbl_df ====
#' data <- colData(rse_small)
#' print(data)
#'
#' ## Automatically move row names to `rowname` column by default.
#' tbl_df <- as(data, Class = "tbl_df")
#'
#' ## tbl_df to DataFrame ====
#' data <- as(tbl_df, Class = "DataFrame")
#' print(data)
#'
#' ## SummarizedExperiment to list ====
#' x <- as(rse_small, Class = "list")
#' class(x)
#' names(x)
NULL



# Internal =====================================================================
# This helps avoid dropping of rowData when coercing from an object that
# inherits from RangedSummarizedExperiment. This will be safe to remove once
# we can fix the bug in SummarizedExperiment.
.asSummarizedExperiment <- function(object) {
    assert_is_all_of(object, "SummarizedExperiment")
    if (is(object, "RangedSummarizedExperiment")) {
        object <- as(object, "RangedSummarizedExperiment")
    }
    as(object, "SummarizedExperiment")
}



# Coerce to tibble =============================================================
# Note that our constructors move rownames to "rowname" column automatically
# by default, to avoid dropping rownames accidentally.



#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble



# S3 constructor.
.as_tibble <-  # nolint
    function(x, rownames = "rowname", ...) {
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
                rownames = rownames,
                ...
            )
        )
    }



#' @importFrom tibble as_tibble
#' @method as_tibble DataFrame
#' @export
as_tibble.DataFrame <- .as_tibble



# The default handling from data.frame isn't clean, so add this.
# Default method will warn: `Arguments in '...' ignored`.
#' @importFrom tibble as_tibble
#' @method as_tibble GRanges
#' @export
as_tibble.GRanges <-  # nolint
    function(x, rownames = "rowname", ...) {
        do.call(
            what = .as_tibble,
            args = list(
                x = as(x, "data.frame"),
                rownames = rownames,
                ...
            )
        )
    }



# S4 constructor.
# Note that we can't declare additional formals in S4 coercion methods, so
# set reasonable defaults above in S3 methods.
.asTibble <- function(from) {
    as_tibble(from)
}



#' @rdname coerce
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = .asTibble
)



#' @rdname coerce
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = .asTibble
)



# Coerce from sparseMatrix =====================================================
#' @method as_tibble GRanges
#' @export
as.data.frame.sparseMatrix <-  # nolint
    function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }



#' @rdname coerce
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "data.frame",
    def = function(from) {
        as.data.frame(from)
    }
)



#' @rdname coerce
#' @name coerce,sparseMatrix,data.frame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = function(from) {
        from %>%
            as("data.frame") %>%
            as("DataFrame")
    }
)



# Coerce from tibble ===========================================================
# Currently only supporting S4 DataFrame here.
#' @rdname coerce
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = function(from) {
        to <- as.data.frame(from, stringsAsFactors = FALSE)
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
)



# Coerce to list ===============================================================
#' @rdname coerce
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



#' @rdname coerce
#' @name coerce,SummarizedExperiment,list-method
setAs(
    from = "SummarizedExperiment",
    to = "list",
    def = coerceS4ToList
)



#' @rdname coerce
#' @export
setMethod(
    f = "as.list",
    signature = signature("SummarizedExperiment"),
    definition = function(x) {
        coerceS4ToList(x)
    }
)
