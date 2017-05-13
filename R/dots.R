#' Objects as dots utilities.
#'
#' Handle object names as dots instead of character vector.
#'
#' @param ... Object as ellipsis.
#' @param .dots Objects as dots.
#'
#' @export
#'
#' @seealso \code{browseVignettes(package = "dplyr")}.
dots <- dplyr:::dots



#' @rdname dots
#' @usage NULL
#' @export
get_objs_from_dots <- devtools:::get_objs_from_dots



#' @rdname dots
#' @export
getObjsFromDots <- get_objs_from_dots
