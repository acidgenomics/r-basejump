# Dot objects ====
# Handle function ellipsis using tidyverse dots method.
# - [browseVignettes("rlang")].
# - [rlang::eval_tidy()].
# - [rlang::dots_values()].
# - [Programming with dplyr](http://dplyr.tidyverse.org/articles/programming.html).

# https://github.com/tidyverse/dplyr/blob/master/R/utils.r
dots <- function(...) {
    eval_bare(substitute(alist(...)))
}

# https://github.com/hadley/devtools/blob/master/R/infrastructure.R
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
