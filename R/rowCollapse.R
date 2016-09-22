#' Collapse rows in a data.frame.
#'
#' @import dplyr
#'
#' @param df long data.frame.
#'
#' @return collapsed data.frame.
#' @export
rowCollapse <- function(df) {
    df %>%
        dplyr::summarise_each(funs(toStringUnique)) %>%
        dplyr::mutate_each(funs(fixNA))
}
