#' Generate syntactically valid names
#'
#' These are convenience function that set [base::names()] on an object and
#' return the object, without modifying the underlying data.
#'
#' - The [makeNames()] and `*Case()` family of functions manipulate a character
#'   vector, supporting either `camelCase` or `snake_case`.
#' - The [setNames()] family of functions work similarly to [makeNames()]
#'   overall, but are to be used on objects supporting name assignments (e.g.
#'   `data.frame`, `matrix`, `list`).
#' - [sanitizeNames()] modifies both [colnames()] and [rownames()] to
#'   `snake_case` format, and is generally recommended for clean-up of RNA-seq
#'   count matrices containing gene names.
#'
#' @rdname names
#'
#' @seealso
#' - [magrittr::set_names()].
#' - [stats::setNames()].
#'
#' @param character Character vector.
#' @param object Object for which a [base::names()] attribute will be
#'   meaningful.
#'
#' @return Character vector or object with valid names.
#' @export
#'
#' @examples
#' # Create character vectors of valid names
#' camel("RNAi clone")
#' snake("RNAi clone")
#'
#' titleCase("RNA sequencing")
#' firstCase("RNA sequencing")
#'
#' # Set names on objects
#' setNamesCamel(head(starwars))
#' setNamesSnake(head(starwars))
#' setNamesDot(head(starwars))
#'
#' # Sanitize both colnames and rownames
#' sanitizeNames(head(mtcars))



#' @rdname names
#' @export
titleCase <- function(character) {
    if (!is.character(character)) {
        stop("character vector required")
    }
    gsub("\\b([a-z])", "\\U\\1", character, perl = TRUE)
}

#' @rdname names
#' @export
title_case <- titleCase



#' @rdname names
#' @export
firstCase <- function(character) {
    character %>%
        titleCase %>%
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE)
}

#' @rdname names
#' @export
first_case <- firstCase



#' @rdname names
#' @export
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

#' @rdname names
#' @export
make_names <- makeNames



#' @rdname names
#' @export
makeNamesCamel <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}

#' @rdname names
#' @export
make_names_camel <- makeNamesCamel

#' @rdname names
#' @export
camel <- makeNamesCamel



#' @rdname names
#' @export
makeNamesSnake <- function(character) {
    character %>%
        makeNames %>%
        str_replace_all("\\.", "_")
}

#' @rdname names
#' @export
make_names_snake <- makeNamesSnake


#' @rdname names
#' @export
snake <- makeNamesSnake



#' @rdname names
#' @export
setNamesCamel <- function(object) {
    setNames(object, makeNamesCamel(names(object)))
}

#' @rdname names
#' @export
set_names_camel <- setNamesCamel



#' @rdname names
#' @export
setNamesDot <- function(object) {
    setNames(object, makeNames(names(object)))
}


#' @rdname names
#' @export
set_names_dot <- setNamesDot



#' @rdname names
#' @export
setNamesSnake <- function(object) {
    setNames(object, makeNamesSnake(names(object)))
}

#' @rdname names
#' @export
set_names_snake <- setNamesSnake



#' @rdname names
#' @usage NULL
#' @export
setColnames <- set_colnames



#' @rdname names
#' @usage NULL
#' @export
setRownames <- set_rownames



#' @rdname names
#' @export
sanitizeNames <- function(object) {
    if (is.null(names(object))) {
        stop("Object doesn't contain names")
    }
    # (Column) names
    names(object) <- makeNamesSnake(names(object))
    # Rows, if they exist
    if (!is.null(rownames(object))) {
        # Ignore numbered rownames
        if (!identical(rownames(object), as.character(1:nrow(object)))) {
            rownames(object) <- makeNamesSnake(rownames(object))
        }
    }
    object
}

#' @rdname names
#' @export
sanitize_names <- sanitizeNames

#' @rdname names
#' @export
sanitiseNames <- sanitizeNames

#' @rdname names
#' @export
sanitise_names <- sanitizeNames
