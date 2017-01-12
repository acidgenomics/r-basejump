#' Collapse rows in a tibble
#'
#' @importFrom dplyr mutate_each summarise_each
#' @importFrom tibble as_tibble
#' @keywords general
#' @param tibble Long \code{tibble}
#' @return Collapsed \code{tibble}
collapse <- function(tibble) {
    tibble %>%
        tibble::as_tibble(.) %>%
        dplyr::summarise_each(funs(toStringUnique)) %>%
        dplyr::mutate_each(funs(fixNA))
}
