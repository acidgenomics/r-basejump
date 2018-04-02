#' `grep` String
#'
#' Generate a [grep()] string for pattern matching against comma separated
#' [toString()] output.
#'
#' @family Atomic Vector Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return `character` string for [grep()] matching.
#' @export
#'
#' @examples
#' grepString("gene")
grepString <- function(object) {
    assert_is_a_string(object)
    object %>%
        paste0(
            # Unique
            "^", ., "$",
            "|",
            # Beginning of list
            "^", ., ",",
            "|",
            # Middle of list
            "\\s", ., ",",
            "|",
            # End of list
            "\\s", ., "$"
        )
}
