#' Markdown Plotlist
#'
#' @rdname mdPlotlist
#' @name mdPlotlist
#' @author Michael Steinbaugh
#'
#' @inheritParams AllGenerics
#'
#' @param headerLevel Header level.
NULL



# Constructors =================================================================
.mdPlotlist <- function(object, headerLevel) {
    if (is.null(names(object))) {
        warning("object does not contain names")
    }
    return <- lapply(seq_along(object), function(a) {
        name <- names(object)[[a]]
        if (is.character(name) && is.numeric(headerLevel)) {
            mdHeader(name, level = headerLevel, asis = TRUE)
        }
        p <- object[[a]]
        show(p)
        p
    })
    invisible(return)
}



# Methods ======================================================================
#' @rdname mdPlotlist
#' @export
setMethod(
    "mdPlotlist",
    signature("list"),
    .mdPlotlist)
