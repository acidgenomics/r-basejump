#' General Arguments
#'
#' @name general
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @param atomic `atomic` vector.
#' @param object Object.
#' @param value Value to assign.
#' @param x Object.
#' @param y Secondary object.
#' @param ... Additional arguments.
#'
#' @param dir Output directory. Defaults to the current working directory.
#' @param envir Environment to use for assignment. Defaults to `parent.frame()`,
#'   which will assign into the calling environment.
#' @param file File path.
#' @param headerLevel Markdown header level.
#' @param interestingGroups `character`. Groups of interest.
#' @param organism Full latin organism name (e.g. "`Homo sapiens`").
#' @param plotlist `list` containing `ggplot` objects.
#' @param return Object class to return. Uses [match.arg()] internally and picks
#'   the first item in the vector by default.
NULL
