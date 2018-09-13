# nolint start
# Using snake case, following ggplot2 conventions.



#' ggplot2 Geometric Objects
#'
#' Convenience functions with modified defaults for
#' [ggplot2](http://ggplot2.org).
#'
#' @section basejump_geom_abline:
#'
#' Horizontal or vertical cutoff line.
#'
#' @section basejump_geom_label:
#'
#' Modified version of [ggplot2::geom_label()].
#'
#' @section basejump_geom_label_average:
#'
#' Add average labels to a plot. For example, `col` can be `nGene`. Median or
#' mean values are always calculated per sample (`sampleName`).
#'
#' @section basejump_geom_label_repel:
#'
#' Repulsive textual annotations. Modified basejump version of
#' [ggrepel::geom_label_repel()]. If advanced customization of the text labels
#' is required, simply use the ggrepel version instead.
#'
#' @name ggplot2_geoms
#' @family ggplot2 Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams ggplot2::geom_label
#' @param color `string`. Text color (e.g. `"orange"`).
#' @param size `scalar integer`. Font size.
#' @param xintercept,yintercept `scalar numeric` denoting x- or y-axis cutoff.
#'   Specify one but not both.
#' @param data `data.frame`. Data.
#' @param col `string`. Column name.
#' @param fun `string`. Function name to use for average calculation. Currently
#'   supports "`mean`" or "`median`".
#' @param digits `scalar integer`. Number of significant digits to use. Defaults
#'   to rounded.
#'
#' @seealso
#' - [ggplot2::geom_label()].
#' - [ggrepel::geom_label_repel()].
#'
#' @return `ggproto`.
#'
#' @examples
#' # basejump_geom_abline ====
#' # x-axis line
#' geom <- basejump_geom_abline(xintercept = 1L)
#' geom
#'
#' # y-axis line
#' geom <- basejump_geom_abline(yintercept = 1L)
#' geom
#'
#' # basejump_geom_label ====
#' geom <- basejump_geom_label()
#' geom
#'
#' # basejump_geom_label_average ====
#' geom <- basejump_geom_label_average(
#'     data = data.frame(
#'         sampleName = c("sample1", "sample2"),
#'         counts = seq_len(8L)
#'     ),
#'     col = "counts",
#'     fun = "mean"
#' )
#' geom
#'
#' # basejump_geom_label_repel ====
#' geom <- basejump_geom_label_repel()
#' geom
NULL



#' @rdname ggplot2_geoms
#' @export
basejump_geom_abline <- function(
    xintercept = NULL,
    yintercept = NULL
) {
    alpha <- 0.75
    color <- "black"
    linetype <- "dashed"
    size <- 1L
    if (
        (is.null(xintercept) && is.null(yintercept)) ||
        (is.numeric(xintercept) && is.numeric(yintercept))
    ) {
        stop("Either `xintercept` or `yintercept` is required")
    } else if (is.numeric(xintercept)) {
        geom_vline(
            xintercept = xintercept,
            alpha = alpha,
            color = color,
            linetype = linetype,
            size = size
        )
    } else if (is.numeric(yintercept)) {
        geom_hline(
            yintercept = yintercept,
            alpha = alpha,
            color = color,
            linetype = linetype,
            size = size
        )
    }
}



#' @rdname ggplot2_geoms
#' @export
basejump_geom_label <- function(
    data = NULL,
    mapping = NULL,
    ...
) {
    do.call(
        what = geom_label,
        args = list(
            data = data,
            mapping = mapping,
            alpha = 0.75,
            color = "white",
            fill = "black",
            fontface = "bold",
            label.padding = unit(0.2, "lines"),
            label.size = NA,
            show.legend = FALSE,
            ...
        )
    )
}



#' @rdname ggplot2_geoms
#' @export
basejump_geom_label_average <- function(
    data,
    col,
    fun = c("mean", "median"),
    digits = 0L,
    ...
) {
    data <- as.data.frame(data)
    assert_is_a_string(col)
    assert_is_subset(col, colnames(data))
    assert_is_an_integer(digits)
    fun <- match.arg(fun)
    fun <- get(fun)
    assert_is_function(fun)

    aggdata <- aggregate(
        formula = as.formula(paste(col, "sampleName", sep = " ~ ")),
        data = data,
        FUN = fun
    )
    aggdata[["roundedAverage"]] <- round(aggdata[[col]], digits = digits)

    # Add `aggregate` column for facet wrapping, if necessary
    if ("aggregate" %in% colnames(data)) {
        sampleFacet <- data %>%
            .[, c("sampleName", "aggregate")] %>%
            unique()
        data <- merge(
            x = aggdata,
            y = sampleFacet,
            by = "sampleName",
            all.x = TRUE
        )
    } else {
        data <- aggdata
    }

    do.call(
        what = basejump_geom_label,
        args = list(
            data = data,
            mapping = aes(label = !!sym("roundedAverage")),
            ...
        )
    )
}



#' @rdname ggplot2_geoms
#' @export
basejump_geom_label_repel <- function(
    data = NULL,
    mapping = NULL,
    color = NULL,
    size = 4L,
    ...
) {
    geom <- do.call(
        what = geom_label_repel,
        args = list(
            data = data,
            mapping = mapping,
            arrow = arrow(length = unit(0.01, "npc")),
            box.padding = unit(0.5, "lines"),
            fill = "white",
            fontface = "bold",
            force = 1L,
            point.padding = unit(0.75, "lines"),
            segment.size = 0.5,
            show.legend = FALSE,
            size = size,
            ...
        )
    )
    if (is.character(color)) {
        geom[["aes_params"]][["colour"]] <- color
    }
    geom
}



# nolint end
