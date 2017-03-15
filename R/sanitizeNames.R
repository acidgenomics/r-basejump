#' Sanitize names
#'
#' This function prevents illegal characters from being set in variable names
#' and column or row names. Additionally, it will prepend an \code{X} character
#' if necessary
#'
#' @author Michael Steinbaugh
#' @keywords dev text
#'
#' @param data \code{data.frame}
#'
#' @return Sanitized \code{data.frame}
#' @export
sanitizeNames <- function(data) {
    names(data) <- names(data) %>%
        sanitize %>%
        # Fix columns leading with a number
        gsub("^([0-9])", "X\\1", .)
    if (is.matrix | is.data.frame) {
        rownames(data) <- rownames(data) %>%
            sanitize
    }
    return(data)
}
