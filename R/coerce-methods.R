#' Methods for Coercing an Object to a Class
#'
#' @section tibble:
#'
#' Coerce an object to a `tibble` (`tbl_df`) using `as(object, Class =
#' "tbl_df")`. Tibbles don't support rowname assignment, so here we are ensuring
#' they are kept by moving them to a column named `rowname` upon coercion. This
#' helps avoid downstream unexpected data loss when using the dplyr chain of
#' single table verbs, such as [dplyr::arrange()], [dplyr::filter()], or
#' [dplyr::mutate()].
#'
#' Conversely, when coercing a `tibble` back to an S4 `DataFrame`, our
#' `as(tbl_df, Class = "DataFrame")` method looks for the "rowname" column and
#' will attempt to move it back to [rownames()] automatically, unless there are
#' duplicates present.
#'
#' @section list:
#'
#' It is often useful to coerce an S4 object to a flat `list` for archival
#' storage. Here we are providing the [coerceS4ToList()] generic, which
#' consistently coerces the slots in any S4 to a standard `list`. Additionally,
#' here we have improved support for `SummarizedExperiment` to `list` coercion,
#' returning the slots as a `list`.
#'
#' @name coerce
#' @aliases as
#' @author Michael Steinbaugh
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
#' - [methods::methods()].
#' - [tibble::as_tibble()].
#' - [as.list()].
#'
#' @examples
#' # DataFrame to tbl_df ====
#' data <- colData(rse_small)
#' print(data)
#'
#' # Automatically move row names to `rowname` column by default.
#' as(data, Class = "tbl_df")
#' as_tibble(data)
#'
#' # tbl_df to DataFrame ====
#' data <- as(tbl_df, "DataFrame")
#' print(data)
#'
#' # SummarizedExperiment to list ====
#' x <- as(rse_small, "list")
#' class(x)
#' names(x)
NULL



# Internal =====================================================================
.asSummarizedExperiment <- function(object) {
    assert_is_all_of(object, "SummarizedExperiment")
    if (is(object, "RangedSummarizedExperiment")) {
        object <- as(object, "RangedSummarizedExperiment")
    }
    as(object, "SummarizedExperiment")
}



# Coerce to tibble =============================================================
# Our constructor helps avoid accidental dropping of rownames.
# Note that the standard tibble methods use `rownames = NULL` instead.
.as_tibble <-  # nolint
    function(x, rownames = "rowname", ...) {
        if (is(x, "tbl_df")) {
            return(x)
        }

        # Require valid columns (atomic, list).
        valid <- vapply(
            X = x,
            FUN = function(x) {
                is.atomic(x) || is.list(x)
            },
            FUN.VALUE = logical(1L),
            USE.NAMES = TRUE
        )
        if (!all(valid)) {
            invalid <- names(valid[!valid])
            stop(paste0(
                "tibble currently atomic and list columns.\n",
                "Invalid columns: ", toString(invalid)
            ), call. = FALSE)
        }

        # Coerce from S4 `DataFrame` to standard `data.frame`.
        # Here we're suppressing the warning about some object classes not
        # supporting the `stringsAsFactors` argument (e.g. `DataFrame`).
        x <- suppressWarnings(
            as.data.frame(x = x, stringsAsFactors = FALSE)
        )

        # Move rownames automatically by default to "rowname" column, unless the
        # object doesn't have rownames defined.
        if (!hasRownames(x)) {
            rownames <- NULL
        }

        as_tibble(x, rownames = rownames)
    }



# S3 methods -------------------------------------------------------------------
#' @importFrom tibble as_tibble
#' @method as_tibble DataFrame
#' @export
as_tibble.DataFrame <- .as_tibble



#' @importFrom tibble as_tibble
#' @method as_tibble GRanges
#' @export
as_tibble.GRanges <- .as_tibble



# S4 methods -------------------------------------------------------------------
.asTibble <- function(from) {
    as_tibble(from)
}



#' @rdname coerce
#' @name coerce,matrix,tbl_df-method
setAs(
    from = "matrix",
    to = "tbl_df",
    def = .asTibble
)



#' @rdname coerce
#' @name coerce,data.frame,tbl_df-method
setAs(
    from = "data.frame",
    to = "tbl_df",
    def = .asTibble
)



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
            length(rownames) &&
            !any(duplicated(rownames))
        ) {
            rownames(to) <- rownames
            to[["rowname"]] <- NULL
        }
        # Warn on invalid dimnames.
        if (!all(validDimnames(to))) {
            warning("Invalid dimnames detected", call. = FALSE)
        }
        to
    }
)



# Coerce to list ===============================================================
.asList <-  # nolint
    function(from) {
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
#' @export
setMethod(
    f = "coerceS4ToList",
    signature = signature("ANY"),
    definition = .asList
)



#' @rdname coerce
#' @name coerce,SummarizedExperiment,list-method
setAs(
    from = "SummarizedExperiment",
    to = "list",
    def = getMethod(
        f = "coerceS4ToList",
        signature = signature(from = "ANY")
    )
)



#' @rdname coerce
#' @export
setMethod(
    f = "as.list",
    signature = signature("SummarizedExperiment"),
    definition = function(x) {
        .asList(x)
    }
)
