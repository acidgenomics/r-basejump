#' Multiple Markdown plots
#'
#' Supports using a named `list` containing multiple `ggplot` objects, which
#' can be used in an R Markdown report, separated by headers. Internally, the
#' headers are generated with the `markdownHeader` function.
#'
#' @export
#' @inheritParams params
#'
#' @param list `list`.
#'   Named list containing `ggplot` objects.
#'
#' @return Graphical output of plots.
#' Separated by Markdown headers.
#'
#' @seealso `cowplot::plot_grid()`.
#'
#' @examples
#' loadRemoteData(url = file.path(basejumpTestsURL, "plotlist.rda"))
#' names(plotlist)
#' markdownPlots(list = plotlist)
markdownPlots <- function(list, headerLevel = 2L) {
    assert(
        is.list(list),
        hasNames(list),
        isHeaderLevel(headerLevel)
    )
    invisible(mapply(
        name = names(list),
        plot = list,
        MoreArgs = list(headerLevel = headerLevel),
        FUN = function(name, plot, headerLevel) {
            assert(isString(name))
            markdownHeader(name, level = headerLevel, asis = TRUE)
            show(plot)
            plot
        },
        SIMPLIFY = FALSE
    ))
}



#' @rdname markdownPlots
#' @usage NULL
#' @export
mdPlots <- markdownPlots
