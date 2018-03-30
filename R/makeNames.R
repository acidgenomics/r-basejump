#' Make Syntactically Valid Names
#'
#' @note Sanitizes names using "`_`" instead of "`.`".
#'
#' @author Michael Steinbaugh
#'
#' @param names `atomic` to be coerced to syntactically valid names. Will be
#'   coerced to `character` if necessary.
#' @param unique `logical`; if `TRUE`, the resulting elements are unique.
#'
#' @seealso `base::make.names()`.
#'
#' @return Character vector.
#' @export
makeNames <- function(names, unique = FALSE) {
    assert_is_atomic(names)
    assert_is_a_bool(unique)
    names <- as.character(names)
    names <- make.names(names, unique = unique)
    names <- gsub("\\.", "_", names)
    names
}
