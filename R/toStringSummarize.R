#' Summarize all rows in a tibble using toString
#' @export
#' @importFrom dplyr mutate_each summarise_each
#' @importFrom tibble as_tibble
#' @keywords general
#' @param tibble Long \code{tibble}
#' @return Collapsed \code{tibble}
toStringSummarize <- function(tibble) {
    tibble %>%
        tibble::as_tibble(.) %>%
        dplyr::summarise_each(funs(toStringUnique)) %>%
        dplyr::mutate_each(funs(fixNA))
}
