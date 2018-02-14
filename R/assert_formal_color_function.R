#' Color Function Formal Assert Check
#'
#' @family Assert Checks
#'
#' @inherit assert
#' @inheritParams general
#'
#' @export
assert_formal_color_function <- function(x) {  # nolint
    assert_is_any_of(x, c("function", "NULL"))
    if (is.function(x)) {
        colors <- x(2L)
        assert_is_character(colors)
        if (!all(is_hex_color(colors))) {
            # viridis adds "FF" to the end of hex colors.
            # Attempt to fix before running hex check.
            colors <- gsub("^(#[A-Z0-9]{6})[A-Z0-9]{2}$", "\\1", colors)
        }
        assert_all_are_hex_colors(colors)
    }
}
