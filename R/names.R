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
#' @name names
#' @usage NULL
#'
#' @param x Character vector or an object for which [names()] assignment will be
#'   meaningful.
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
NULL






# Vector operations ====
makeTitleCase <- function(x) {
    x %>%
        as.character %>%
        gsub("\\b([a-z])", "\\U\\1", ., perl = TRUE)
}



makeFirstCase <- function(x) {
    x %>%
        makeTitleCase %>%
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE)
}



makeNamesDotted <- function(x) {
    x %>%
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



makeNamesCamel <- function(x) {
    x %>%
        makeNamesDotted %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}



makeNamesSnake <- function(x) {
    x %>%
        makeNamesDotted %>%
        str_replace_all("\\.", "_")
}






# Object name operations ====
checkNames <- function(x) {
    if (!is.null(names(x))) {
        TRUE
    } else {
        FALSE
    }
}



checkRownames <- function(x) {
    if (!is.null(rownames(x))) {
        # Ignore numbered rownames
        if (!identical(rownames(x), as.character(seq_len(nrow(x))))) {
            TRUE
        } else {
            FALSE
        }
    } else {
        FALSE
    }
}



setNamesDotted <- function(x, rownames) {
    if (checkNames(x)) {
        x <- setNames(x, makeNamesDotted(names(x)))
    }
    if (isTRUE(rownames) & checkRownames(x)) {
        x <- setRownames(x, makeNamesDotted(rownames(x)))
    }
    x
}



setNamesCamel <- function(x, rownames) {
    if (checkNames(x)) {
        x <- setNames(x, makeNamesCamel(names(x)))
    }
    if (isTRUE(rownames) & checkRownames(x)) {
        x <- setRownames(x, makeNamesCamel(rownames(x)))
    }
    x
}



setNamesSnake <- function(x, rownames) {
    if (checkNames(x)) {
        x <- setNames(x, makeNamesSnake(names(x)))
    }
    if (isTRUE(rownames) & checkRownames(x)) {
        x <- setRownames(x, makeNamesSnake(rownames(x)))
    }
    x
}






# Flexible utilities ====
#' @rdname names
#' @export
camel <- function(x, rownames = TRUE) {
    if (is.null(names(x))) {
        makeNamesCamel(x)
    } else {
        setNamesCamel(x, rownames)
    }
}



#' @rdname names
#' @export
dotted <- function(x, rownames = TRUE) {
    if (is.null(names(x))) {
        makeNamesDotted(x)
    } else {
        setNamesDotted(x, rownames)
    }
}



#' @rdname names
#' @export
snake <- function(x, rownames = TRUE) {
    if (is.null(names(x))) {
        makeNamesSnake(x)
    } else {
        setNamesSnake(x, rownames)
    }
}



#' @rdname camel_aliases
#' @export
sanitizeNames <- snake

#' @rdname snake_aliases
#' @usage NULL
#' @export
sanitize_names <- sanitizeNames  # nolint

#' @rdname british_aliases
#' @usage NULL
#' @export
sanitiseNames <- snake

#' @rdname british_aliases
#' @usage NULL
#' @export
sanitise_names <- snake  # nolint






# Additional simple name utilities ====
#' @rdname names
#' @usage NULL
#' @export
titleCase <- function(x) {
    if (is.null(names(x))) {
        x <- makeTitleCase(x)
    }
    x
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
title_case <- titleCase  # nolint



#' @rdname names
#' @usage NULL
#' @export
firstCase <- function(x) {
    if (is.null(names(x))) {
        x <- makeFirstCase(x)
    }
    x
}

#' @rdname snake_aliases
#' @usage NULL
#' @export
first_case <- firstCase  # nolint
