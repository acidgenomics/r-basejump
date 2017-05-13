#' Wash a data frame.
#'
#' This function cleans poorly formed separators, leading and trailing commas or
#' spaces, empty cells, and sets \code{NA} values if necessary.
#'
#' @param df Dirty data frame.
#'
#' @return Clean data frame.
#' @export
wash <- function(df) {
    if (!is.data.frame(df)) {
        stop("Object must be a data frame")
    }
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
    mutate_all(df, funs(gsubs))
}
