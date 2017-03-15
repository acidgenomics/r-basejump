#' @keywords internal
utils::globalVariables(".")



# https://github.com/hadley/devtools/blob/master/R/utils.r
#' @keywords internal
dots <- function(...) {
    eval(substitute(alist(...)))
}



# https://github.com/hadley/devtools/blob/master/R/infrastructure.R
#' @keywords internal
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
