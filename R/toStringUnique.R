#' Convert to a Unique Character String
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @seealso [base::toString()].
#'
#' @return `character` string.
#' @export
#'
#' @examples
#' vec <- c("hello", "world", NA, "hello", "world", NA)
#' toStringUnique(vec)
toStringUnique <- function(object) {
    assert_is_atomic(object)
    object %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        toString()
}
