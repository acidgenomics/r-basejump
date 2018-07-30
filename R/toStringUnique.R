#' Convert to a Unique Character String
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @seealso [base::toString()].
#'
#' @return `string`.
#' @export
#'
#' @examples
#' toStringUnique(c("hello", "world", NA, "hello", "world", NA))
toStringUnique <- function(x) {
    assert_is_atomic(x)
    x %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        toString()
}
