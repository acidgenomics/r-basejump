# nolint start



#' Evaluate Dots
#'
#' Capture function ellipses as dots using
#' [rlang](https://cran.r-project.org/package=rlang) syntax.
#'
#' @keywords internal
#'
#' @param ... Ellipsis.
#' @param .dots Dots values.
#'
#' @seealso
#' - `browseVignettes("rlang")`.
#' - [rlang::dots_values()], [rlang::eval_tidy()].
#' - https://github.com/tidyverse/dplyr/blob/master/R/utils.r
#' - https://github.com/hadley/devtools/blob/master/R/infrastructure.R
#'
#' @export
dots <- function(...) {
    eval_bare(substitute(alist(...)))
}



#' @rdname dots
#' @export
get_objs_from_dots <- function(.dots) {
    if (length(.dots) == 0L) {
        stop("Nothing to save", call. = FALSE)
    }

    is_name <- vapply(.dots, is.symbol, logical(1))
    if (any(!is_name)) {
        stop("Can only save existing named objects", call. = FALSE)
    }

    objs <- vapply(.dots, as.character, character(1))
    duplicated_objs <- which(stats::setNames(duplicated(objs), objs))
    if (length(duplicated_objs) > 0L) {
        objs <- unique(objs)
        warning("Saving duplicates only once: ",
                paste(names(duplicated_objs), collapse = ", "),
                call. = FALSE)
    }
    objs
}



#' @rdname dots
#' @usage NULL
#' @export
get_objs_from_dots -> getObjsFromDots



# nolint end
