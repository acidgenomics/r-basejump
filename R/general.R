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
#' @param dir `string`. Output directory. Defaults to the current working
#'   directory.
#' @param envir `environment` to use for assignment. Defaults to
#'   `parent.frame()`, which will assign into the calling environment.
#' @param file `string`. File path.
#' @param genes `character`. Gene identifiers.
#' @param headerLevel `scalar integer`. Markdown header level (1-7).
#' @param interestingGroups `character`. Groups of interest.
#' @param organism `string`. Full latin organism name (e.g. "`Homo sapiens`").
#' @param plotlist `list` containing `ggplot` objects.
#' @param return Object class to return. Uses [match.arg()] internally and picks
#'   the first item in the vector by default.
NULL
