#' Complete Themes
#'
#' [ggplot2](http://ggplot2.tidyverse.org) themes.
#'
#' @name themes
#' @family Plot Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams ggplot2::element_text
#'
#' @return `theme`.
#'
#' @seealso [ggplot2::theme()].
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(
#'     data = mpg,
#'     mapping = aes(
#'         x = manufacturer,
#'         y = displ,
#'         color = manufacturer
#'     )
#' ) +
#'     geom_point()
NULL



# Midnight =====================================================================
#' @rdname themes
#' @export
#'
#' @section Midnight:
#' Blackout theme that sets the plot background as black, with white text.
#' Inspired by `Seurat::DarkTheme()`, with some color modifications. Useful
#' for visualizing many points with a high dynamic color range, such as t-SNE
#' expression plots.
#'
#' @examples
#' # Midnight Theme
#' p + theme_midnight(aspect_ratio = 1L, legend_position = "none")
theme_midnight <- function(  # nolint
    base_size = 14L,
    base_family = "",
    base_face = c("bold", "plain"),
    aspect_ratio = NULL,
    legend_position = c("bottom", "right", "none"),
    grid = TRUE
) {
    assert_is_a_number(base_size)
    assert_is_a_string(base_family)
    base_face <- match.arg(base_face)
    legend_position <- match.arg(legend_position)
    assert_is_a_bool(grid)

    gray <- "gray12"  #1F1F1F

    text <- element_text(
        family = base_family,
        face = base_face,
        color = "white"
    )

    if (isTRUE(grid)) {
        axis_line <- element_blank()
        axis_ticks <- element_blank()
        grid <- element_line(color = gray)
    } else {
        axis_line <- element_line(color = "white")
        axis_ticks <- element_line(color = "white")
        grid <- element_blank()
    }

    theme_minimal(
        base_size = base_size,
        base_family = base_family
    ) +
        theme(
            text = text,
            aspect.ratio = aspect_ratio,
            axis.line = axis_line,
            axis.text = text,
            axis.text.x = element_text(angle = 90L, hjust = 1L, vjust = 0.5),
            axis.ticks = axis_ticks,
            legend.key = element_rect(color = NA, fill = gray),
            legend.position = legend_position,
            panel.grid.major = grid,
            panel.grid.minor = element_blank(),
            plot.background = element_rect(color = NA, fill = "black"),
            strip.text = text,
            complete = TRUE,
            validate = TRUE
        )
}



# Paperwhite ===================================================================
#' @rdname themes
#' @export
#'
#' @section Paperwhite:
#' High contrast black and white theme optimized for print. Recommended for
#' scientific manuscripts and website tutorials.
#'
#' @examples
#' p + theme_paperwhite(aspect_ratio = 1L, legend_position = "none")
theme_paperwhite <- function(  # nolint
    base_size = 14L,
    base_family = "",
    base_face = c("bold", "plain"),
    aspect_ratio = NULL,
    legend_position = c("bottom", "right", "none"),
    grid = FALSE
) {
    assert_is_a_number(base_size)
    assert_is_a_string(base_family)
    base_face <- match.arg(base_face)
    legend_position <- match.arg(legend_position)
    assert_is_a_bool(grid)

    gray <- "gray88"  #E0E0E0

    text <- element_text(
        family = base_family,
        face = base_face,
        color = "black"
    )

    if (isTRUE(grid)) {
        axis_line <- element_blank()
        axis_ticks <- element_blank()
        grid <- element_line(color = gray)
    } else {
        axis_line <- element_line(color = "black")
        axis_ticks <- element_line(color = "black")
        grid <- element_blank()
    }

    theme_classic(
        base_size = base_size,
        base_family = base_family
    ) +
        theme(
            text = text,
            aspect.ratio = aspect_ratio,
            axis.line = axis_line,
            axis.text = text,
            axis.text.x = element_text(angle = 90L, hjust = 1L, vjust = 0.5),
            axis.ticks = axis_ticks,
            panel.grid.major = grid,
            legend.position = legend_position,
            strip.text = text,
            complete = TRUE,
            validate = TRUE
        )
}



# Aliases ======================================================================
#' @rdname themes
#' @usage NULL
#' @export
midnightTheme <- theme_midnight



#' @rdname themes
#' @usage NULL
#' @export
paperwhiteTheme <- theme_paperwhite
