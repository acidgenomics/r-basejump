#' Make Syntactically Valid Names
#'
#' @note Sanitizes names using "`_`" instead of "`.`".
#'
#' @author Michael Steinbaugh
#'
#' @inheritParams base::make.names
#'
#' @seealso [base::make.names()].
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
