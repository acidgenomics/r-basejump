#' Dynamic Plotlist
#'
#' @rdname dynamicPlotlist
#' @name dynamicPlotlist
#' @author Michael Steinbaugh
#'
#' @param headerLevel Markdown header level. Only applicable when
#'   `return = "markdown`.
#'
#' @return
#' - `grid`: [cowplot::plot_grid()].
#' - `list`: [list].
#' - `markdown`: Markdown output, with headers for each plot.
NULL



# Constructors =================================================================
#' @importFrom cowplot plot_grid
.dynamicPlotlist <- function(
    object,
    return = "grid",
    headerLevel = 2L) {
    if (return == "grid") {
        plot_grid(plotlist = plotlist)
    } else if (return == "list") {
        plotlist
    } else if (return == "markdown") {
        mdPlotlist(plotlist, headerLevel = headerLevel)
    }
}



# Methods ======================================================================
#' @rdname dynamicPlotlist
#' @export
setMethod(
    "dynamicPlotlist",
    signature("list"),
    .dynamicPlotlist)
