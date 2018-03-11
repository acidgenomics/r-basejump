#' Markdown Plotlist
#'
#' @name markdownPlotlist
#' @family R Markdown Functions
#'
#' @inheritParams general
#' @param headerLevel Header level.
#'
#' @return Graphical output of plots, separated by Markdown headers.
#'
#' @seealso [cowplot::plot_grid()].
#'
#' @examples
#' loadRemoteData("http://basejump.seq.cloud/plotlist.rda")
#' markdownPlotlist(plotlist)
NULL



# Methods ======================================================================
#' @rdname markdownPlotlist
#' @export
setMethod(
    "markdownPlotlist",
    signature("list"),
    function(object, headerLevel = 2L) {
        assert_is_list(object)
        assert_has_names(object)
        assertIsAHeaderLevel(headerLevel)
        invisible(mapply(
            name = names(object),
            plot = object,
            MoreArgs = list(headerLevel = headerLevel),
            FUN = function(name, plot, headerLevel) {
                assert_is_a_string(name)
                markdownHeader(name, level = headerLevel, asis = TRUE)
                show(plot)
                plot
            },
            SIMPLIFY = FALSE,
            USE.NAMES = TRUE
        ))
    }
)



# Aliases ======================================================================
#' @rdname markdownPlotlist
#' @export
mdPlotlist <- function(...) {
    markdownPlotlist(...)  # nocov
}
