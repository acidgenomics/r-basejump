#' Wash data.frame.
#'
#' @import magrittr
#' @import stringr
#' @import tibble
#'
#' @param data A \code{data.frame} with leading/trailing spaces/commas, empty cells.
#'
#' @return A reformatted, clean \code{data.frame}.
#' @export
wash <- function(data) {
    data %>%
        # Leading commas:
        apply(., 2, function(a) {
            gsub("^(,|\\s//)\\s(.*)", "\\2", a)
        }) %>%
        # Trailing commas:
        apply(., 2, function(a) {
            gsub("(.*)(,|\\s//)\\s$", "\\1", a)
        }) %>%
        # Duplicate NAs from \code{toString}:
        apply(., 2, function(a) {
            gsub("NA,\\s|,\\sNA", "", a)
        }) %>%
        # NA needed:
        apply(., 2, function(a) {
            gsub("^$|^\\s+$|^NA$", NA, a)
        }) %>%
        tibble::as_tibble(.)
}
