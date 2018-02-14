#' Dynamic Plotlist
#'
#' @rdname dynamicPlotlist
#' @name dynamicPlotlist
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @param return Return type. "grid", "list", and "markdown" are supported.
#' @param headerLevel Markdown header level. Only applicable when
#'   `return = "markdown"`.
#'
#' @seealso [markdownPlotlist].
#'
#' @return
#' - `grid`: [cowplot::plot_grid()].
#' - `list`: [list].
#' - `markdown`: Markdown output, with headers for each plot.
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/plotlist.rda")
#'
#' # Grid
#' dynamicPlotlist(plotlist, return = "grid")
#'
#' # List
#' list <- dynamicPlotlist(plotlist, return = "list")
#' names(list)
#'
#' # Markdown
#' dynamicPlotlist(plotlist, return = "markdown")
NULL



# Constructors =================================================================
#' @importFrom cowplot plot_grid
.dynamicPlotlist <- function(
    object,
    return = "grid",
    headerLevel = 2L) {
    assert_is_a_string(return)
    assert_is_subset(return, c("grid", "list", "markdown"))
    assert_formal_header_level(headerLevel)
    if (return == "grid") {
        plot_grid(plotlist = object)
    } else if (return == "list") {
        object
    } else if (return == "markdown") {
        markdownPlotlist(object, headerLevel = headerLevel)
    }
}



# Methods ======================================================================
#' @rdname dynamicPlotlist
#' @export
setMethod(
    "dynamicPlotlist",
    signature("list"),
    .dynamicPlotlist)
