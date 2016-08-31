#' Cruft removal for tibbles.
#'
#' @import tibble
#'
#' @param crufty Crufty \code{tibble} with leading/trailing spaces, dashes.
#'
#' @return A reformatted, clean \code{tibble}
#' @export
cruft <- function(crufty) {
  crufty %>%
    apply(., 2, function(x) gsub("^$|^ $", NA, x)) %>%
    apply(., 2, function(x) gsub("^(,|\\s//)\\s(.*)", "\\2", x)) %>%
    apply(., 2, function(x) gsub("(.*)(,|\\s//)\\s$", "\\1", x)) %>%
    as_tibble
}
