#' Summarize all rows in a tibble using toString
#'
#' @author Michael Steinbaugh
#' @keywords general
#'
#' @import dplyr
#' @import tibble
#'
#' @param tibble Long \code{tibble}
#'
#' @return Collapsed \code{tibble}
#' @export
toStringSummarize <- function(tibble) {
    tibble %>%
        tibble::as_tibble(.) %>%
        dplyr::summarise_each(funs(toStringUnique)) %>%
        dplyr::mutate_each(funs(fixNA))
}
