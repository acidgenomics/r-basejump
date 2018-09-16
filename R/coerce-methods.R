#' Methods for Coercing an Object to a Class
#'
#' @name coerce
#' @aliases as
#' @author Michael Steinbaugh
#' @importFrom methods coerce
#' @exportMethod coerce
#'
#' @section `tbl_df` (tibble):
#'
#' Coerce an object to a `tibble` using `as(object, Class = "tbl_df")`. Tibbles
#' don't support rowname assignment, so here we are ensuring they are kept by
#' moving them to a column named `rowname` upon coercion. This helps avoid
#' downstream unexpected data loss when using the dplyr chain of single table
#' verbs, such as [dplyr::arrange()], [dplyr::filter()], or [dplyr::mutate()].
#'
#' Conversely, when coercing a `tibble` back to an S4 `DataFrame`, our
#' `as(tbl_df, Class = "DataFrame")` method looks for the "rowname" column and
#' will attempt to move it back to [rownames()] automatically, unless there are
#' duplicates present.
#'
#' @section `list`:
#'
#' It is often useful to coerce an S4 object to a flat `list` for archival
#' storage. Here we are providing the [coerceS4ToList()] generic, which
#' consistently coerces the slots in any S4 to a standard `list`. Additionally,
#' here we have improved support for `SummarizedExperiment` to `list` coercion,
#' returning the slots as a `list`.
#'
#' @return Object of new class.
#'
#' @seealso
#' - [methods::as()].
#' - [methods::canCoerce()].
#' - [S4Vectors::as.list()].
#' - [tibble::tibble()], [tibble::as_tibble()].
#'
#' @examples
#' # DataFrame to tbl_df ====
#' # Automatically move row names to `rowname` column.
#' data <- colData(rse_small)
#' class(data)
#' hasRownames(data)
#'
#' tbl_df <- as(data, Class = "tbl_df")
#' hasRownames(tbl_df)
#' print(tbl_df)
#'
#' # tbl_df to DataFrame ====
#' data <- as(tbl_df, "DataFrame")
#' hasRownames(data)
#' class(data)
#'
#' # SummarizedExperiment to list ====
#' x <- as(rse_small, "list")
#' class(x)
#' names(x)
NULL



# Internal =====================================================================
.coerceToSummarizedExperiment <- function(object) {
    assert_is_all_of(object, "SummarizedExperiment")
    if (is(object, "RangedSummarizedExperiment")) {
        object <- as(object, "RangedSummarizedExperiment")
    }
    as(object, "SummarizedExperiment")
}



# Coerce to tibble =============================================================
# Helps avoid dropping rownames during tidyverse function calls.
.coerceToTibble <- function(from) {
    if (is_tibble(from)) {
        return(from)  # nocov
    }
    # Suppressing the warning about some object classes not supporting the
    # `stringsAsFactors` argument (e.g. DataFrame).
    from <- suppressWarnings(as.data.frame(
        x = from,
        stringsAsFactors = FALSE
    ))
    assert_has_colnames(from)
    if (hasRownames(from)) {
        from <- rownames_to_column(from)
    }
    as_tibble(from)
}

#' @rdname coerce
#' @name coerce,matrix,tbl_df-method
setAs(from = "matrix", to = "tbl_df", def = .coerceToTibble)

#' @rdname coerce
#' @name coerce,data.frame,tbl_df-method
setAs(from = "data.frame", to = "tbl_df", def = .coerceToTibble)

#' @rdname coerce
#' @name coerce,DataFrame,tbl_df-method
setAs(from = "DataFrame", to = "tbl_df", def = .coerceToTibble)

#' @rdname coerce
#' @name coerce,GRanges,tbl_df-method
setAs(from = "GRanges", to = "tbl_df", def = .coerceToTibble)



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
.coerceS4ToList <-  # nolint
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
    definition = .coerceS4ToList
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
#' @importFrom S4Vectors as.list
#' @export
setMethod(
    f = "as.list",
    signature = signature("SummarizedExperiment"),
    definition = function(x) {
        .coerceS4ToList(x)
    }
)
