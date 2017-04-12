#' Make syntactically valid names out of character vectors
#'
#' @rdname makeNames
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector
#'
#' @return Character vector formatted with valid names



#' @rdname makeNames
#' @export
#' @examples
#' makeNames("RNAi clone")
makeNames <- function(character) {
    character %>%
        as.character %>%
        # Convert non-alphanumeric characters
        gsub("[^[:alnum:]]", ".", .) %>%
        # Combine multiple underscores
        gsub("[\\.]+", ".", .) %>%
        # Strip leading or trailing underscores
        gsub("(^\\.|\\.$)", "", .) %>%
        # Special names
        gsub("(m|nc|r)RNA", "\\1rna", .) %>%
        # Convert acronyms to mixed case
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
        # Make first letter lowercase
        gsub("(^[A-Z]{1})", "\\L\\1", ., perl = TRUE) %>%
        # Convert camelCase
        gsub("([a-z0-9])([A-Z])", "\\1.\\L\\2", ., perl = TRUE) %>%
        # Ensure syntactically valid names
        make.names %>%
        # Lowercase everything
        tolower
}



#' @rdname makeNames
#' @export
#' @examples
#' makeNamesCamel("RNAi clone")
makeNamesCamel <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}



#' @rdname makeNames
#' @export
#' @examples
#' makeNamesSnake("RNAi clone")
makeNamesSnake <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.", "_", .)
}



#' @rdname makeNames
#' @export
camel <- makeNamesCamel



#' @rdname makeNames
#' @export
snake <- makeNamesSnake
