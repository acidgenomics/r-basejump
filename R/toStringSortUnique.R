#' toString call that outputs sorted uniques
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @param vector \code{vector}
#'
#' @return Sorted unique \code{string}
#' @export
toStringSortUnique <- function(vector) {
    vector %>%
        unique %>%
        sort %>%
        toString %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
