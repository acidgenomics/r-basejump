#' Collapse rows in a data.frame.
#'
#' @importFrom dplyr mutate_each summarise_each
#'
#' @param tibble long tibble.
#'
#' @return collapsed tibble.
#' @export
collapse <- function(tibble) {
    tibble %>%
        dplyr::summarise_each(funs(toStringUnique)) %>%
        dplyr::mutate_each(funs(fixNA))
}
