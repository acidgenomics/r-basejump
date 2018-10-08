#' General Arguments
#'
#' @name general
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @param object Object.
#' @param value Value to assign.
#' @param x Object.
#' @param ... Additional arguments.
#'
#' @param assay `scalar`. Name or index of count matrix slotted in [assays()].
#'   When passing in a `string`, the name must be defined in [assayNames()].
#' @param check `boolean`. Perform assert checks.
#' @param color `ggproto`/`ScaleDiscrete` or `NULL`. Desired ggplot2 color
#'   scale. Must supply discrete values. When set to `NULL`, the default ggplot2
#'   color palette will be used. If manual color definitions are desired, we
#'   recommend using [ggplot2::scale_color_manual()]. To set the discrete color
#'   palette globally, use `options(basejump.discrete.color =
#'   ggplot2::scale_color_viridis_d())`.
#' @param countsAxisLabel `string`. Counts axis label.
#' @param dir `string`. Output directory. Defaults to the current working
#'   directory.
#' @param envir `environment` to use for assignment. Defaults to
#'   `parent.frame()`, which will assign into the calling environment.
#' @param file `string`. File path.
#' @param fill `ggproto`/`ScaleDiscrete` or `NULL`. Desired ggplot2 fill scale.
#'   Must supply discrete values. When set to `NULL`, the default ggplot2 color
#'   palette will be used. If manual color definitions are desired, we recommend
#'   using [ggplot2::scale_fill_manual()]. To set the discrete fill palette
#'   globally, use `options(bcbio.discrete.fill = scale_fill_viridis_d())`.
#' @param flip `boolean`. Flip x and y axes. Recommended for quality control
#'   plots containing many samples.
#' @param genes `character`. Gene identifiers.
#' @param headerLevel `scalar integer`. Markdown header level (1-7).
#' @param interestingGroups `character`. Groups of interest.
#' @param legend `boolean`. Show plot legend.
#' @param limit `scalar numeric`. Threshold to denote on the plot, using a
#'   dashed line.
#' @param minCounts `scalar integer`. Minimum number of counts per gene in the
#'   count matrix.
#' @param n `scalar integer`. Number to include.
#' @param organism `string`. Full latin organism name (e.g. "`Homo sapiens`").
#' @param perMillion `boolean`. Display as counts per million.
#' @param plotlist `list` containing `ggplot` objects.
#' @param return Object class to return. Uses [match.arg()] internally and picks
#'   the first item in the vector by default.
#' @param title `string`. Plot title.
#' @param trans `string`. Name of the axis scale transformation to apply. See
#'   `help("scale_x_continuous", "ggplot2")` for more information.
#' @param url `string`. Uniform Resource Locator (URL). HTTP or FTP address.
#' @param verbose `boolean`. Run the function with verbose messages? It is only
#'   recommended to enable this when debugging.
#'
#' @param .test `boolean`. *For unit testing only*.
NULL
