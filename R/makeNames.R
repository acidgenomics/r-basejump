#' Make syntactically valid names out of character vectors.
#'
#' @param character Character vector.
#'
#' @return Character vector formatted with valid names.
#' @export
#'
#' @examples
#' makeNames("RNAi clone")
#' makeNamesCamel("RNAi clone")
#' makeNamesSnake("RNAi clone")
makeNames <- function(character) {
    character %>%
        as.character %>%
        # Convert non-alphanumeric characters
        str_replace_all("[^[:alnum:]]", ".") %>%
        # Combine multiple underscores
        str_replace_all("[\\.]+", ".") %>%
        # Strip leading or trailing underscores
        str_replace_all("(^\\.|\\.$)", "") %>%
        # Special names
        str_replace_all("(m|nc|r)RNA", "\\1rna") %>%
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
make_names <- makeNames



# camelCase ====
#' @rdname makeNames
#' @export
makeNamesCamel <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}



#' @rdname makeNames
#' @export
make_names_camel <- makeNamesCamel



# snake_case ====
#' @rdname makeNames
#' @export
makeNamesSnake <- function(character) {
    character %>%
        makeNames %>%
        str_replace_all("\\.", "_")
}



#' @rdname makeNames
#' @export
make_names_snake <- makeNamesSnake



# Shorthand ====
#' @rdname makeNames
#' @export
camel <- makeNamesCamel



#' @rdname makeNames
#' @export
snake <- makeNamesSnake
