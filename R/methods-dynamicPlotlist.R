#' Dynamic Plotlist
#'
#' @rdname dynamicPlotlist
#' @name dynamicPlotlist
#' @author Michael Steinbaugh
#'
#' @param return Return type. "grid", "list", and "markdown" are supported.
#' @param headerLevel Markdown header level. Only applicable when
#'   `return = "markdown"`.
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
    validReturn <- c("grid", "list", "markdown")
    if (!return %in% validReturn) {
        stop(paste(
            "'return' must contain:", toString(validReturn)
        ), call. = FALSE)
    }

    if (return == "grid") {
        plot_grid(plotlist = object)
    } else if (return == "list") {
        object
    } else if (return == "markdown") {
        mdPlotlist(object, headerLevel = headerLevel)
    }
}



# Methods ======================================================================
#' @rdname dynamicPlotlist
#' @export
setMethod(
    "dynamicPlotlist",
    signature("list"),
    .dynamicPlotlist)
