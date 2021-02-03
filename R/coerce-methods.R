#' Coerce to list
#'
#' @rdname coerce-list
#' @name coerceToList
#' @note Don't attempt to defined methods for `as.list`.
#' @note Updated 2020-01-18.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso
#' Refer to the S4Vectors package for details.
#' - `help(topic = "Annotated-class", package = "S4Vectors")`
#' - `help(topic = "List-class", package = "S4Vectors")`
#' - `showClass("Annnotated")`.
#' - `showClass("List")`.
#' - `methods::coerce`.
#'
#' @return `list`.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#'
#' ## SummarizedExperiment ====
#' x <- RangedSummarizedExperiment
#' x <- coerceToList(x)
#' class(x)
#' names(x)
NULL



## Updated 2019-10-22.
`coerceToList,Annotated` <-  # nolint
    function(object) {
        out <- lapply(slotNames(object), function(slot) {
            if (.hasSlot(object, slot)) {
                slot(object, slot)
            } else {
                NULL  # nocov
            }
        })
        names(out) <- slotNames(object)
        out
    }



#' @rdname coerce-list
#' @export
setMethod(
    f = "coerceToList",
    signature = signature("Annotated"),
    definition = `coerceToList,Annotated`
)
