#' Wash data.frame.
#'
#' @import dplyr
#'
#' @param tibble A \code{data.frame} with leading/trailing spaces/commas, empty cells.
#'
#' @return A reformatted, clean \code{data.frame}.
#' @export
wash <- function(data) {
    gsubs <- function(a) {
        a %>%
            # Duplicate separators
            gsub("(,|;|/)\\s(,|;|/)", "\\1", .) %>%
            # Leading separators
            gsub("^(,|;|/)\\s", "", .) %>%
            # Trailing separators
            gsub("\\s(,|;|/)$", "", .) %>%
            # NAs in string
            gsub("NA,\\s|,\\sNA", "", .) %>%
            # Character NAs
            gsub("^$|^\\s+$|^NA$", NA, .)
        return(a)
    }
    dplyr::mutate_each(data, funs(gsubs))
}
