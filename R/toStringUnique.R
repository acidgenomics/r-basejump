#' Convert to a Unique Character String
#'
#' @family Sanitization Functions
#' @export
#'
#' @param object `atomic`.
#'
#' @seealso [base::toString()].
#'
#' @return `string`.
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
