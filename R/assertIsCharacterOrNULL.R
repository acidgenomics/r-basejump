#' Character Vector or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
assertIsCharacterOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("character", "NULL"),
        severity = severity)
}



# Aliases ======================================================================
#' @rdname assertIsCharacterOrNULL
#' @export
assertIsCharacterOrNULL -> assert_is_character_or_null
