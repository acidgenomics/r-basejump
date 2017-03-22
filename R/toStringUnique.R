#' toString call that outputs sorted uniques
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @param vector \code{vector}
#'
#' @return Unique \code{string}
#' @export
toStringUnique <- function(vector) {
    vector %>%
        unique %>%
        toString %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
