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
    assert_is_list(object)
    assert_has_names(object)
    .assert_markdown_header_level(level)
    invisible(lapply(seq_along(object), function(a) {
        name <- names(object)[[a]]
        if (is.character(name) && is.numeric(headerLevel)) {
            markdownHeader(name, level = headerLevel, asis = TRUE)
        }
        p <- object[[a]]
        show(p)
        p
    }))
}



# Methods ======================================================================
#' @rdname markdownPlotlist
#' @export
setMethod(
    "markdownPlotlist",
    signature("list"),
    .markdownPlotlist)
