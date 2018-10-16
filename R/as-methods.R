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
#' @inheritParams general
#'
#' @return Object of new class.
#'
#' @seealso
#' - [methods::as()].
#' - [methods::canCoerce()].
#' - [methods::showMethods()].
#' - [utils::methods()].
#' - [as.list()].
#'
#' @examples
#' data(rse_small)
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



# Coerce from sparseMatrix =====================================================
#' @method as.data.frame sparseMatrix
#' @export
as.data.frame.sparseMatrix <-  # nolint
    function(x, ...) {
        as.data.frame(as.matrix(x), ...)
    }



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
        from %>%
            as("data.frame") %>%
            as("DataFrame")
    }
)



# Coerce to list ===============================================================
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
#' @name coerce,SummarizedExperiment,list-method
setAs(
    from = "SummarizedExperiment",
    to = "list",
    def = coerceS4ToList
)



#' @rdname as
#' @export
setMethod(
    f = "as.list",
    signature = signature("SummarizedExperiment"),
    definition = function(x) {
        coerceS4ToList(x)
    }
)
