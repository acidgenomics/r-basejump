#' Is Hex Color Function or NULL Assert Check
#'
#' @family Assert Checks
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsHexColorFunctionOrNULL(viridis)
assertIsHexColorFunctionOrNULL <- function(x, severity = "stop") {
    assert_is_any_of(
        x = x,
        classes = c("function", "NULL"),
        severity = severity)
    if (is.function(x)) {
        colors <- x(2L)
        assert_is_character(colors, severity = severity)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running hex check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors, severity = severity)
    }
}



# Aliases ======================================================================
#' @rdname assertIsHexColorFunctionOrNULL
#' @export
assertIsHexColorFunctionOrNULL -> assert_is_hex_color_function_or_null
