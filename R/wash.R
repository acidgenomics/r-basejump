#' Wash a data frame.
#'
#' This function cleans poorly formed separators, leading and trailing commas or
#' spaces, empty cells, and sets `NA` values if necessary.
#'
#' @param df Dirty data frame.
#'
#' @return Clean data frame.
#' @export
wash <- function(df) {
    if (!is.data.frame(df)) {
        stop("Object must be a data frame")
    }
    replace <- function(a) {
        a %>%
            # Duplicate separators
            str_replace_all("(,|;|/)\\s(,|;|/)", "\\1") %>%
            # Leading separators
            str_replace_all("^(,|;|/)\\s", "") %>%
            # Trailing separators
            str_replace_all("\\s(,|;|/)$", "") %>%
            # NAs in string
            str_replace_all("NA,\\s|,\\sNA", "") %>%
            # Character NAs
            gsub("^$|^\\s+$|^NA$", NA, .)
    }
    mutate_all(df, funs(replace))
}
