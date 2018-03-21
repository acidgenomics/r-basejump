#' Make Syntactically Valid Names
#'
#' @note Sanitizes names using "`_`" instead of "`.`".
#'
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @inherit base::make.names
#'
#' @return Character vector.
#' @export
makeNames <- function(names, unique = FALSE) {
    assert_is_character(names)
    assert_is_a_bool(unique)
    names <- make.names(names, unique = unique)
    names <- gsub("\\.", "_", names)
    names
}
