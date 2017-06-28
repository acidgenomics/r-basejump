# Character vector operations ====
.makeTitleCase <- function(object) {
    object %>%
        as.character %>%
        gsub("\\b([a-z])", "\\U\\1", ., perl = TRUE)
}

.makeFirstCase <- function(object) {
    object %>%
        .makeTitleCase %>%
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE)
}

.makeNamesDotted <- function(object) {
    object %>%
        as.character %>%
        make.names %>%
        # Convert non-alphanumeric characters
        str_replace_all("[^[:alnum:]]", ".") %>%
        # Combine multiple underscores
        str_replace_all("[\\.]+", ".") %>%
        # Strip leading or trailing dots
        str_replace_all("(^\\.|\\.$)", "") %>%
        # Special acronym exceptions
        str_replace_all("RNAi", "Rnai") %>%
        # Handle snakeCase acronyms
        # e.g. worfdbHTMLRemap -> worfdb.html.remap
        gsub("([A-Z])([A-Z]+)([A-Z])([a-z])",
             "\\1\\L\\2\\U\\3\\L\\4", ., perl = TRUE) %>%
        # Convert remaining acronyms to mixed case
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
        # Make first letter lowercase
        gsub("(^[A-Z]{1})", "\\L\\1", ., perl = TRUE) %>%
        # Convert camelCase
        gsub("([a-z0-9])([A-Z])", "\\1.\\L\\2", ., perl = TRUE) %>%
        tolower
}

.makeNamesCamel <- function(object) {
    object %>%
        .makeNamesDotted %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}

.makeNamesSnake <- function(object) {
    object %>%
        .makeNamesDotted %>%
        str_replace_all("\\.", "_")
}



# Object name operations ====
.checkNames <- function(object) {
    if (!is.null(names(object))) {
        TRUE
    } else {
        FALSE
    }
}

.checkRownames <- function(object) {
    if (!is.null(rownames(object))) {
        # Ignore numbered rownames
        if (!identical(rownames(object), as.character(seq_len(nrow(object))))) {
            TRUE
        } else {
            FALSE
        }
    } else {
        FALSE
    }
}

.setNamesDotted <- function(object, rownames) {
    if (.checkNames(object)) {
        object <- setNames(object, .makeNamesDotted(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- setRownames(object, .makeNamesDotted(rownames(object)))
    }
    object
}

.setNamesCamel <- function(object, rownames) {
    if (.checkNames(object)) {
        object <- setNames(object, .makeNamesCamel(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- setRownames(object, .makeNamesCamel(rownames(object)))
    }
    object
}

.setNamesSnake <- function(object, rownames) {
    if (.checkNames(object)) {
        object <- setNames(object, .makeNamesSnake(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- setRownames(object, .makeNamesSnake(rownames(object)))
    }
    object
}






#' Make syntactically valid names
#'
#' These are convenience functions that sanitize names into `camelCase`
#' (preferred), `snake_case`, or `dot.notation`. Dot assignment is provided for
#' use with base R operations, but we advise against using `dot.notation` name
#' assignments for values and/or function.
#'
#' For unnamed character vectors, these functions will sanitize the underlying
#' values. Otherwise, the functions will set [names()] and/or [rownames()] on
#' objects supporting name assignments. They return the object without
#' modification of the underlying data.
#'
#' @rdname names
#'
#' @param object Character vector or an object for which [names()] assignment
#'   will be meaningful.
#' @param rownames Include row names.
#'
#' @return Object with syntatically valid names. For objects supporting
#'   [base::names()], the underlying data returns unchanged.
#'
#' @examples
#' # Unnamed character vector, with no names assigned
#' unnamed_vec <- c("hello world",
#'                  "HELLO WORLD",
#'                  "RNAi clones",
#'                  "worfdbHTMLRemap",
#'                  123,
#'                  NA)
#'
#' # Named character vector
#' named_vec <- c(Item.A = "hello world", Item.B = "HELLO WORLD")
#'
#' # Data frame with colnames and rownames
#' df <- head(mtcars)
#'
#' # Tibble, with colnames. Rownames aren't supported on these!
#' tbl <- head(starwars)
#'
#' # List, with assigned namess
#' lst <- list(Item.A = c(1, 2),
#'             Item.B = c(3, 4))
#'
#'
#' # camelCase
#' camel(unnamed_vec)
#' camel(named_vec)
#' camel(df)
#' camel(df, rownames = FALSE)
#' camel(tbl)
#' camel(lst)
#'
#'
#' # snake_case
#' snake(unnamed_vec)
#' snake(named_vec)
#' snake(df)
#' snake(df, rownames = FALSE)
#' snake(tbl)
#' snake(lst)
#'
#'
#' # dotted.case
#' dotted(unnamed_vec)
#' dotted(named_vec)
#' dotted(df)
#' dotted(df, rownames = FALSE)
#' dotted(tbl)
#' dotted(lst)



#' @rdname names
#' @export
camel <- function(object, rownames = TRUE) {
    if (is.null(names(object))) {
        .makeNamesCamel(object)
    } else {
        .setNamesCamel(object, rownames)
    }
}



#' @rdname names
#' @export
dotted <- function(object, rownames = TRUE) {
    if (is.null(names(object))) {
        .makeNamesDotted(object)
    } else {
        .setNamesDotted(object, rownames)
    }
}



#' @rdname names
#' @export
snake <- function(object, rownames = TRUE) {
    if (is.null(names(object))) {
        .makeNamesSnake(object)
    } else {
        .setNamesSnake(object, rownames)
    }
}



#' @rdname names
#' @usage NULL
#' @export
sanitizeNames <- snake

#' @rdname names
#' @usage NULL
#' @export
sanitizeNames -> sanitiseNames

#' @rdname names
#' @usage NULL
#' @export
sanitizeNames -> sanitize_names  # nolint

#' @rdname names
#' @usage NULL
#' @export
sanitizeNames -> sanitise_names  # nolint



#' @rdname names
#' @export
titleCase <- function(object) {
    if (is.null(names(object))) {
        object <- .makeTitleCase(object)
    }
    object
}

#' @rdname names
#' @usage NULL
#' @export
titleCase -> title_case  # nolint



#' @rdname names
#' @export
firstCase <- function(object) {
    if (is.null(names(object))) {
        object <- .makeFirstCase(object)
    }
    object
}

#' @rdname names
#' @usage NULL
#' @export
firstCase -> first_case  # nolint
