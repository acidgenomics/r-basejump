#' Make names camelCase
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector
#'
#' @return Character vector formatted as camelCase
#' @export
#'
#' @examples
#' makeNamesCamel("RNAi clone")
makeNamesCamel <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}


#' @rdname makeNamesCamel
#' @export
camel <- makeNamesCamel
