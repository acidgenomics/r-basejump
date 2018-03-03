#' Dynamic Plotlist
#'
#' @rdname dynamicPlotlist
#' @name dynamicPlotlist
#' @family Plot Functions
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



# Methods ======================================================================
#' @rdname dynamicPlotlist
#' @importFrom cowplot plot_grid
#' @export
setMethod(
    "dynamicPlotlist",
    signature("list"),
    function(
        object,
        return = "grid",
        headerLevel = 2L) {
        assert_is_a_string(return)
        assert_is_subset(return, c("grid", "list", "markdown"))
        assertIsAHeaderLevel(headerLevel)
        if (return == "grid") {
            plot_grid(plotlist = object)
        } else if (return == "list") {
            object
        } else if (return == "markdown") {
            markdownPlotlist(object, headerLevel = headerLevel)
        }
    }
)
