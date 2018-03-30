#' Markdown Plotlist
#'
#' @family R Markdown Functions
#'
#' @param plotlist List containing `ggplot` objects.
#' @param headerLevel Header level.
#'
#' @return Graphical output of plots, separated by Markdown headers.
#' @export
#'
#' @seealso [cowplot::plot_grid()].
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/plotlist.rda")
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



# Aliases ======================================================================
#' @rdname markdownPlotlist
#' @export
markdownPlotlist -> mdPlotlist
