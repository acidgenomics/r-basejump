#' Convert to a Unique Character String
#'
#' @family Atomic Vector Functions
#'
#' @inheritParams general
#'
#' @seealso [base::toString()].
#'
#' @return string.
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
