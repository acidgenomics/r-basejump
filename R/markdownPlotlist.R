#' Markdown Plotlist
#'
#' Supports using a named `list` containing multiple `ggplot` objects, which
#' can be used in an R Markdown report, separated by headers. Internally, the
#' headers are generated with the [markdownHeader()] function.
#'
#' @family R Markdown Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return Graphical output of plots, separated by Markdown headers.
#'
#' @seealso [cowplot::plot_grid()].
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/plotlist.rda")
#' names(plotlist)
#' markdownPlotlist(plotlist)
markdownPlotlist <- function(
    plotlist,
    headerLevel = 2L
) {
    assert_is_list(plotlist)
    assert_has_names(plotlist)
    assertIsAHeaderLevel(headerLevel)
    invisible(mapply(
        name = names(plotlist),
        plot = plotlist,
        MoreArgs = list(headerLevel = headerLevel),
        FUN = function(name, plot, headerLevel) {
            assert_is_a_string(name)
            markdownHeader(name, level = headerLevel, asis = TRUE)
            show(plot)
            plot
        },
        SIMPLIFY = FALSE
    ))
}



#' @rdname markdownPlotlist
#' @export
markdownPlotlist -> mdPlotlist
