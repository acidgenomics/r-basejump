#' Convert to a Unique Character String
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#'
#' @param object `atomic`.
#'
#' @seealso [base::toString()].
#'
#' @return `string`.
#' @export
#'
#' @examples
#' toStringUnique(c("hello", "world", NA, "hello", "world", NA))
toStringUnique <- function(object) {
    assert_is_atomic(object)
    object %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        toString()
}
