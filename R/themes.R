# nolint begin
# snake_case, following ggplot2 conventions



#' Complete Themes
#'
#' [ggplot2](http://ggplot2.tidyverse.org) themes.
#'
#' @name themes
#' @family Plot Functions
#' @author Michael Steinbaugh
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param face Font face ("`bold`", "`plain`").
#' @param aspect_ratio Numeric scalar specifying the plot proportions. Use `1`
#'   for a perfectly square plot (including the axis labels).
#' @param legend_position Legend key position. We're being a little more
#'   restrictive here, only allowing "`bottom`", "`right`", or "`none`".
#'   Including the legend at the top or the left side of the plot rarely makes
#'   sense and is discouraged.
#' @param grid Label the major panel grids. If `TRUE`, the axis lines will also
#'   be removed.
#'
#' @return `theme`.
#'
#' @seealso [ggplot2::theme()].
#'
#' @examples
#' library(ggplot2)
#'
#' # Example ggplot object
#' p <- ggplot(
#'     data = mpg,
#'     mapping = aes(
#'         x = manufacturer,
#'         y = displ,
#'         colour = manufacturer
#'     )
#' ) +
#'     geom_point()
NULL



# Midnight =====================================================================
#' @rdname themes
#' @export
#'
#' @section Midnight Theme:
#' Blackout theme that sets the plot background as black, with white text.
#' Inspired by `Seurat::DarkTheme()`, with some color modifications. Useful
#' for visualizing many points with a high dynamic color range, such as t-SNE
#' expression plots.
#'
#' @examples
#' # Midnight Theme
#' p + theme_midnight(aspect_ratio = 1L, legend_position = "none")
theme_midnight <- function(
    base_size = 14L,
    base_family = "",
    face = c("bold", "plain"),
    aspect_ratio = NULL,
    legend_position = c("bottom", "right", "none"),
    grid = TRUE
) {
    assert_is_a_number(base_size)
    assert_is_a_string(base_family)
    face <- match.arg(face)
    assertIsANumberOrNULL(aspect_ratio)
    legend_position <- match.arg(legend_position)
    assert_is_a_bool(grid)

    gray <- "gray8"

    text <- element_text(
        family = base_family,
        face = face,
        colour = "white"
    )

    if (isTRUE(grid)) {
        axis_line <- element_blank()
        axis_ticks <- element_blank()
        grid <- element_line(colour = gray)
    } else {
        axis_line <- element_line(colour = "white")
        axis_ticks <- element_line(colour = "white")
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
            legend.key = element_rect(colour = NA, fill = gray),
            legend.position = legend_position,
            panel.grid.major = grid,
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA, fill = "black"),
            strip.text = text,
            complete = TRUE,
            validate = TRUE
        )
}



# Paperwhite ===================================================================
#' @rdname themes
#' @export
#'
#' @section Paperwhite Theme:
#' High contrast black and white theme optimized for print. Recommended for
#' scientific manuscripts and website tutorials.
#'
#' @examples
#' p + theme_paperwhite(aspect_ratio = 1L, legend_position = "none")
theme_paperwhite <- function(
    base_size = 14L,
    base_family = "",
    face = c("bold", "plain"),
    aspect_ratio = NULL,
    legend_position = c("bottom", "right", "none"),
    grid = FALSE
) {
    assert_is_a_number(base_size)
    assert_is_a_string(base_family)
    face <- match.arg(face)
    assertIsANumberOrNULL(aspect_ratio)
    legend_position <- match.arg(legend_position)
    assert_is_a_bool(grid)

    gray <- "gray92"

    text <- element_text(
        family = base_family,
        face = face,
        colour = "black"
    )

    if (isTRUE(grid)) {
        axis_line <- element_blank()
        axis_ticks <- element_blank()
        grid <- element_line(colour = gray)
    } else {
        axis_line <- element_line(colour = "black")
        axis_ticks <- element_line(colour = "black")
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
            strip.background = element_rect(colour = NA, fill = "white"),
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



# nolint end
