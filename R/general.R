#' General Arguments
#'
#' @name general
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @param object Object.
#' @param value Value to assign.
#' @param ... Additional arguments.
#'
#' @param color `ggproto`/`ScaleDiscrete` or `NULL`. Desired ggplot2 color
#'   scale. Must supply discrete values. When set to `NULL`, the default ggplot2
#'   color palette will be used. If manual color definitions are desired, we
#'   recommend using [ggplot2::scale_color_manual()].
#'   To set the discrete color palette globally, use
#'   `options(basejump.discrete.color = ggplot2::scale_color_viridis_d())`.
#' @param dir `string`. Output directory. Defaults to the current working
#'   directory.
#' @param envir `environment` to use for assignment. Defaults to
#'   `parent.frame()`, which will assign into the calling environment.
#' @param file `string`. File path.
#' @param genes `character`. Gene identifiers.
#' @param headerLevel `scalar integer`. Markdown header level (1-7).
#' @param interestingGroups `character`. Groups of interest.
#' @param legend `boolean`. Show plot legend.
#' @param organism `string`. Full latin organism name (e.g. "`Homo sapiens`").
#' @param plotlist `list` containing `ggplot` objects.
#' @param return Object class to return. Uses [match.arg()] internally and picks
#'   the first item in the vector by default.
#' @param verbose `boolean`. Run the function with verbose messages? It is only
#'   recommended to enable this when debugging.
NULL
