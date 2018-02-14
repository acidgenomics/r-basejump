#' Character Vector or NULL Assert Check
#'
#' @family Assert Checks
#'
#' @inherit assert
#' @inheritParams general
#'
#' @export
assert_is_character_or_null <- function(x) {  # nolint
    assert_is_any_of(x, c("character", "NULL"))
}
