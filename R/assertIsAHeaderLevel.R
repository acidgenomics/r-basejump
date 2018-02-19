#' Is a Markdown Header Level Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' # Markdown supports levels 1-7
#' assertFormalHeaderLevel(1L)
#' tryCatch(
#'     assertFormalHeaderLevel(8L),
#'     error = function(e) e)
assertIsAHeaderLevel <- function(x, severity = "stop") {
    assert_is_a_number(x, severity = severity)
    assert_is_subset(
        x = as.integer(x),
        y = seq(1L:7L),
        severity = severity)
}



# Aliases ======================================================================
#' @rdname assertIsAHeaderLevel
#' @export
assertIsAHeaderLevel -> assert_is_a_header_level
