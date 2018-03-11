#' Assert Checks
#'
#' @name assert
#' @family Assert Check Functions
#' @keywords internal
#'
#' @param x Object.
#' @param envir Environment.
#' @param inherits Should the enclosing frames of the environment be searched?
#' @param severity How severe should the consequences of the assertion be?
#'   Either "`stop`", "`warning`", "`message`", or "`none`".
#'
#' @return Abort on mismatch.
NULL



.assertFormalEnsembldbReturn <- function(x, severity = "stop") {
    assert_is_a_string(x, severity = severity)
    assert_is_subset(
        x = x,
        y = ensemblReturn,
        severity = severity
    )
}



.assertFormalMakeNames <- function(x, severity = "stop") {
    assert_is_a_string(x, severity = severity)
    assert_is_subset(
        x = x,
        y = c("camel", "dotted", "snake" , "upperCamel"),
        severity = severity
    )
}



.assertFormalSeverity <- function(x, severity = "stop") {
    assert_is_a_string(x, severity = severity)
    assert_is_subset(
        x = x,
        y = c("stop", "warning", "message", "none")
    )
}
