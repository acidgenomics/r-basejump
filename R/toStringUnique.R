#' toString call that outputs sorted uniques
#' @export
#' @keywords general
#' @param vector \code{vector}
#' @return Unique \code{string}
toStringUnique <- function(vector) {
    vector %>%
        unique %>%
        toString %>%
        gsub("NA,\\s|,\\sNA", "", .)
}
