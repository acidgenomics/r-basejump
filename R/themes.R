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
#'
#' # Example ggplot object
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
#' p + midnightTheme(aspectRatio = 1L, legendPosition = "none")
midnightTheme <- function(
    baseSize = 14L,
    baseFamily = "",
    baseFace = c("bold", "plain"),
    aspectRatio = NULL,
    legendPosition = c("bottom", "right", "none"),
    grid = TRUE
) {
    f <- camel(formals())
    .assignCamelArgs()
    assert_is_a_number(baseSize)
    assert_is_a_string(baseFamily)
    # call to character
    assert_is_subset(
        x = baseFace,
        y = as.character(f[["baseFace"]])[-1L]
    )
    baseFace <- baseFace[[1L]]
    # call to character
    assert_is_subset(
        x = legendPosition,
        y = as.character(f[["legendPosition"]])[-1L]
    )
    legendPosition <- legendPosition[[1L]]
    assert_is_a_bool(grid)

    gray <- "gray12"  #1F1F1F

    text <- element_text(
        family = baseFamily,
        face = baseFace,
        color = "white"
    )

    if (isTRUE(grid)) {
        axisLine <- element_blank()
        axisTicks <- element_blank()
        grid <- element_line(color = gray)
    } else {
        axisLine <- element_line(color = "white")
        axisTicks <- element_line(color = "white")
        grid <- element_blank()
    }

    theme_minimal(
        base_size = baseSize,
        base_family = baseFamily
    ) +
        theme(
            text = text,
            aspect.ratio = aspectRatio,
            axis.line = axisLine,
            axis.text = text,
            axis.text.x = element_text(angle = 90L, hjust = 1L, vjust = 0.5),
            axis.ticks = axisTicks,
            legend.key = element_rect(color = NA, fill = gray),
            legend.position = legendPosition,
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
#' p + paperwhiteTheme(aspectRatio = 1L, legendPosition = "none")
paperwhiteTheme <- function(
    baseSize = 14L,
    baseFamily = "",
    baseFace = c("bold", "plain"),
    aspectRatio = NULL,
    legendPosition = c("bottom", "right", "none"),
    grid = FALSE
) {
    f <- camel(formals())
    .assignCamelArgs()
    assert_is_a_number(baseSize)
    assert_is_a_string(baseFamily)
    # call to character
    assert_is_subset(
        x = baseFace,
        y = as.character(f[["baseFace"]])[-1L]
    )
    baseFace <- baseFace[[1L]]
    # call to character
    assert_is_subset(
        x = legendPosition,
        y = as.character(f[["legendPosition"]])[-1L]
    )
    legendPosition <- legendPosition[[1L]]
    assert_is_a_bool(grid)

    gray <- "gray88"  #E0E0E0

    text <- element_text(
        family = baseFamily,
        face = baseFace,
        color = "black"
    )

    if (isTRUE(grid)) {
        axisLine <- element_blank()
        axisTicks <- element_blank()
        grid <- element_line(color = gray)
    } else {
        axisLine <- element_line(color = "black")
        axisTicks <- element_line(color = "black")
        grid <- element_blank()
    }

    theme_classic(
        base_size = baseSize,
        base_family = baseFamily
    ) +
        theme(
            text = text,
            aspect.ratio = aspectRatio,
            axis.line = axisLine,
            axis.text = text,
            axis.text.x = element_text(angle = 90L, hjust = 1L, vjust = 0.5),
            axis.ticks = axisTicks,
            panel.grid.major = grid,
            legend.position = legendPosition,
            strip.text = text,
            complete = TRUE,
            validate = TRUE
        )
}



# Aliases ======================================================================
#' @rdname themes
#' @export
theme_midnight <- midnightTheme  # nolint
formals(theme_midnight) <- snake(formals(midnightTheme))



#' @rdname themes
#' @export
theme_paperwhite <- paperwhiteTheme  # nolint
formals(theme_paperwhite) <- snake(formals(paperwhiteTheme))
