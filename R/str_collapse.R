#' Stringr collapse call for dplyr summarize.
#' @import stringr
#' @param x Input.
#' @return Output.
#' @export
str_collapse <- function(x) {
    if (length(x) > 1) {
        y <- x %>%
            unique %>%
            str_sort %>%
            str_c(collapse = ", ")
    } else {
        y <- x
    }
    return(y)
}
