#' Convert to a Unique Character String
#'
#' @family Vector Functions
#'
#' @importFrom stats na.omit
#'
#' @inheritParams general
#'
#' @seealso [base::toString()].
#'
#' @return String.
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
