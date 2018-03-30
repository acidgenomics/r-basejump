#' Make Syntactically Valid Names
#'
#' @note Sanitizes names using underscore instead of dot.
#'
#' @author Michael Steinbaugh
#'
#' @param names `atomic` to be coerced to syntactically valid names. Will be
#'   coerced to `character` if necessary.
#' @param unique `logical`; if `TRUE`, the resulting elements are unique.
#'
#' @return Character vector.
#' @export
#'
#' @seealso [base::make.names()].
#'
#' @examples
#' makeNames(c(123L, "hello world"))
makeNames <- function(names, unique = FALSE) {
    assert_is_atomic(names)
    assert_is_a_bool(unique)
    names <- as.character(names)
    names <- make.names(names, unique = unique)
    names <- gsub("\\.", "_", names)
    names
}
