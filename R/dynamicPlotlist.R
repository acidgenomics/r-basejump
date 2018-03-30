#' Dynamic Plotlist
#'
#' @family Plot Functions
#'
#' @inheritParams general
#' @param headerLevel Markdown header level. Only applicable when
#'   `return = "markdown"`.
#' @param return Return type. "`grid`", "`list`", and "`markdown`" are
#'   supported.
#'
#' @seealso [markdownPlotlist()].
#'
#' @return
#' - `grid`: [cowplot::plot_grid()].
#' - `list`: `list`.
#' - `markdown`: Markdown output, with headers for each plot.
#' @export
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
dynamicPlotlist <- function(
    plotlist,
    headerLevel = 2L,
    return = c("grid", "list", "markdown")
) {
    assert_is_list(plotlist)
    assertIsAHeaderLevel(headerLevel)
    return <- match.arg(return)
    if (return == "grid") {
        plot_grid(plotlist = object)
    } else if (return == "list") {
        object
    } else if (return == "markdown") {
        markdownPlotlist(object, headerLevel = headerLevel)
    }
}
