#' Midnight Theme
#'
#' Blackout palette that sets the plot background as black, with white text.
#'
#' @importFrom ggplot2 element_blank element_line element_rect element_text
#'   theme theme_minimal
#'
#' @param ... Passthrough arguments to [ggplot2::theme()].
#'
#' @seealso
#' - [ggplot2::theme()].
#' - Inspired by `Seurat::DarkTheme()`, with some modifications.
#'
#' @return ggplot theme.
#' @export
midnightTheme <- function(...) {
    gray <- "gray12"
    blackBackground <- element_rect(color = NA, fill = "black")
    grayBackground <- element_rect(color = NA, fill = gray)
    grayLine <- element_line(color = gray)
    whiteText <- element_text(color = "white")
    theme_minimal() +
        theme(
            text = whiteText,
            axis.line = element_blank(),
            axis.text = whiteText,
            axis.ticks = element_blank(),
            legend.key = grayBackground,
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = grayLine,
            panel.grid.minor = element_blank(),
            plot.background = blackBackground,
            complete = TRUE,
            validate = TRUE,
            ...)
}
