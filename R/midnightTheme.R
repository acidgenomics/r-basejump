#' Midnight Theme
#'
#' [ggplot2](http://ggplot2.tidyverse.org) blackout palette that sets the plot
#' background as black, with white text.
#'
#' @family Plot Functions
#' @author Michael Steinbaugh
#'
#' @return `theme`.
#' @export
#'
#' @seealso
#' - [ggplot2::theme()].
#' - Inspired by `Seurat::DarkTheme()`, with some modifications.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars) +
#'     geom_point(aes(x = wt, y = mpg, colour = factor(gear)))
#' p + midnightTheme()
midnightTheme <- function() {
    gray <- "gray12"
    blackBackground <- element_rect(color = NA, fill = "black")
    grayBackground <- element_rect(color = NA, fill = gray)
    grayLine <- element_line(color = gray)
    whiteText <- element_text(color = "white")
    theme_minimal() +
        theme(
            text = whiteText,
            axis.text = whiteText,
            legend.key = grayBackground,
            panel.grid.major = grayLine,
            panel.grid.minor = element_blank(),
            plot.background = blackBackground,
            strip.text = whiteText,
            complete = TRUE,
            validate = TRUE
        )
}



# Aliases ======================================================================
#' @rdname midnightTheme
#' @usage NULL
#' @export
midnightTheme -> theme_midnight  # nolint
