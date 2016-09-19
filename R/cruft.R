#' Cruft removal for tibbles.
#'
#' @import dplyr
#' @import tibble
#'
#' @param data Data with leading/trailing spaces/commas, empty cells.
#'
#' @return A reformatted, clean \code{tibble}
#' @export
cruft <- function(data) {
    data %>%
        # Set cells with only spaces to NA
        apply(., 2, function(a) {
            gsub("^$|^ $", NA, a)
            }) %>%
        # Fix leading and trailing commas
        apply(., 2, function(a) {
            gsub("^(,|\\s//)\\s(.*)", "\\2", a)
        }) %>%
        apply(., 2, function(a) {
            gsub("(.*)(,|\\s//)\\s$", "\\1", a)
        }) %>%
        tibble::as_tibble
}
