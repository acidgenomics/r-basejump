#' @name as
#' @aliases coerce
#' @inherit methods::as title description references
#' @importFrom methods coerce
#' @exportMethod coerce
#'
#' @section list:
#'
#' It is often useful to coerce an S4 object to a flat `list` for archival
#' storage. Here we are providing the `coerceS4ToList()` function, which
#' consistently coerces the slots in any S4 to a standard `list`. Additionally,
#' here we have improved support for `SummarizedExperiment` to `list` coercion,
#' returning the slots as a `list`.
#'
#' @section tibble:
#'
#' Coerce an object to a tibble (`tbl_df`, `data.frame`) using either S3 or S4
#' methods:
#'
#' - S3: `as_tibble(x)` (or `as_tibble()` alias).
#' - S4: `as(object, Class = "tbl_df")`.
#'
#' Tibbles don't support row name assignment, so here we are ensuring they are
#' kept by moving them to a column named `rowname` by default. This helps avoid
#' downstream unexpected data loss when using the dplyr chain of single table
#' verbs, such as `dplyr::arrange()`, `dplyr::filter()`, or `dplyr::mutate()`.
#'
#' This behavior can be overriden in the S3 method by setting `rowname = NULL`
#' instead, which is the current default in the tibble package. The S4 coercion
#' method doesn't support arguments, and therefore always attempts to move
#' rownames automatically, if defined.
#'
#' Conversely, when coercing a `tibble` back to an S4 `DataFrame`, our
#' `as(tbl_df, Class = "DataFrame")` method looks for the `rowname` column and
#' will attempt to move it back to `base::rownames()` automatically, unless
#' there are duplicates present.
#'
#' @inheritParams params
#'
#' @return Object of new class.
#'
#' @seealso
#' - `methods::as()`.
#' - `methods::canCoerce()`.
#' - `methods::showMethods()`.
#' - `tibble::tibble()`.
#' - `utils::methods()`.
#'
#' @examples
#' data(rse)
#'
#' ## DataFrame to tbl_df ====
#' data <- SummarizedExperiment::colData(rse)
#' print(data)
#'
#' x <- as(data, "tbl_df")
#' print(x)
#'
#' x <- tibble::as_tibble(data)
#' print(x)
#'
#' ## GRanges to tbl_df ====
#' data <- SummarizedExperiment::rowRanges(rse)
#'
#' x <- as(data, "tbl_df")
#'
#' x <- tibble::as_tibble(data)
#' colnames(x)
#'
#' ## sparseMatrix to data.frame ====
#' data(sparse)
#'
#' x <- BiocGenerics::as.data.frame(sparse)
#' class(x)
#'
#' x <- as(sparse, "data.frame")
#' class(x)
#'
#' ## tbl_df to DataFrame ====
#' data <- tibble::as_tibble(datasets::iris)
#' x <- as(data, "DataFrame")
NULL



# as.data.frame : S4 ===========================================================
#' @importFrom BiocGenerics as.data.frame
#' @export
BiocGenerics::as.data.frame



# If we include this in usage it breaks pkgdown.
# @inheritParams BiocGenerics::as.data.frame
#' @rdname as
#' @usage NULL
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



# DataFrame : S3 (internal) ====================================================
as.DataFrame <-  # nolint
    function(x) {
        UseMethod("as.DataFrame")
    }



as.DataFrame.default <- function(x) {
    to <- as.data.frame(x, stringsAsFactors = FALSE)
    to <- as(to, "DataFrame")
    rownames <- as.character(to[["rowname"]])
    if (
        length(rownames) > 0L &&
        !any(duplicated(rownames))
    ) {
        rownames(to) <- rownames
        to[["rowname"]] <- NULL
    }
    to
}



# DataFrame : S4 ===============================================================
#' @rdname as
#' @name coerce,sparseMatrix,DataFrame-method
setAs(
    from = "sparseMatrix",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)

#' @rdname as
#' @name coerce,tbl_df,DataFrame-method
setAs(
    from = "tbl_df",
    to = "DataFrame",
    def = function(from) {
        as.DataFrame(from)
    }
)



# SummarizedExperiment : S3 (internal) =========================================
as.SummarizedExperiment <-  # nolint
    function(x) {
        UseMethod("as.SummarizedExperiment")
    }



# Note that our method here keeps track of `rowData` when coercing an object
# that extends RangedSummarizedExperiment to SummarizedExperiment. This bug
# needs to be fixed in the SummarizedExperiment package.
as.SummarizedExperiment.default <-  # nolint
    function(x) {
        if (is(x, "RangedSummarizedExperiment")) {
            rowMeta <- metadata(rowRanges(x))
            x <- as(x, "RangedSummarizedExperiment")
        } else {
            rowMeta <- metadata(rowData(x))
        }
        x <- as(x, "SummarizedExperiment")
        metadata(rowData(x)) <- rowMeta
        x
    }



# tibble : S3 ==================================================================
#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble



#' @method as_tibble DataFrame
#' @export
as_tibble.DataFrame <-  # nolint
    function(x, ..., rownames = "rowname") {
        # Coerce to standard data frame.
        x <- as(x, "data.frame")
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



# tibble : S4 ==================================================================
setOldClass(Classes = "tbl_df")



#' @rdname as
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)

#' @rdname as
#' @name coerce,DataFrame,tbl_df-method
setAs(
    from = "DataFrame",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)

#' @rdname as
#' @name coerce,GRanges,tbl_df-method
setAs(
    from = "GRanges",
    to = "tbl_df",
    def = function(from) {
        as_tibble(from)
    }
)
