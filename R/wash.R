#' Wash
#'
#' This function cleans poorly formed separators, leading and trailing commas or
#' spaces, empty cells, and sets \code{NA} values if necessary.
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @import dplyr
#'
#' @param df Dirty data frame
#'
#' @return Washed data frame
#' @export
wash <- function(df) {
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
    }
    mutate_each(df, funs(gsubs))
}
