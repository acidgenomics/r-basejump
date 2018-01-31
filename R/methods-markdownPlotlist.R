#' Markdown Plotlist
#'
#' @rdname markdownPlotlist
#' @name markdownPlotlist
#' @author Michael Steinbaugh
#'
#' @inheritParams AllGenerics
#'
#' @param headerLevel Header level.
#'
#' @seealso [cowplot::plot_grid()].
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/plotlist.rda")
#' markdownPlotlist(plotlist)
NULL



# Constructors =================================================================
.markdownPlotlist <- function(object, headerLevel = 2L) {
    if (is.null(names(object))) {
        warn("Object does not contain names")
    }
    return <- lapply(seq_along(object), function(a) {
        name <- names(object)[[a]]
        if (is.character(name) && is.numeric(headerLevel)) {
            markdownHeader(name, level = headerLevel, asis = TRUE)
        }
        p <- object[[a]]
        show(p)
        p
    })
    invisible(return)
}



# Methods ======================================================================
#' @rdname markdownPlotlist
#' @export
setMethod(
    "markdownPlotlist",
    signature("list"),
    .markdownPlotlist)
