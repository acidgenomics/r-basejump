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
#' toStringUnique(c("hello", "world", NA, "hello", "world", NA))
toStringUnique <- function(atomic) {
    assert_is_atomic(atomic)
    atomic %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        toString()
}
