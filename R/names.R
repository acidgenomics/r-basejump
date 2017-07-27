#' Make Syntactically Valid Names
#'
#' These are convenience functions that sanitize names into `camelCase`
#' (**preferred**), `snake_case`, or `dotted.case`. `dotted.case` support is
#' provided for matching against base R parameters, but we strongly advise
#' against using it for object and/or argument name assignments.
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
#' unnamed_vec <-
#'   c("hello world",
#'     "HELLO WORLD",
#'     "RNAi clones",
#'     "worfdbHTMLRemap",
#'     123,
#'     NA)
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
#' camel(df, rownames = TRUE)
#' camel(tbl)
#' camel(lst)
#'
#'
#' # snake_case
#' snake(unnamed_vec)
#' snake(named_vec)
#' snake(df)
#' snake(df, rownames = TRUE)
#' snake(tbl)
#' snake(lst)
#'
#'
#' # dotted.case
#' dotted(unnamed_vec)
#' dotted(named_vec)
#' dotted(df)
#' dotted(df, rownames = TRUE)
#' dotted(tbl)
#' dotted(lst)



# General object name operations ====
#' @rdname names
#' @usage NULL
.checkNames <- function(object) {
    if (!is.null(names(object))) {
        TRUE
    } else {
        FALSE
    }
}

#' @rdname names
#' @usage NULL
.checkRownames <- function(object) {
    if (!is.null(rownames(object))) {
        # Ignore numbered rownames
        if (!identical(rownames(object),
                       as.character(seq_len(nrow(object))))) {
            TRUE
        } else {
            FALSE
        }
    } else {
        FALSE
    }
}



# dotted.case ====
#' @rdname names
#' @usage NULL
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

#' @rdname names
#' @usage NULL
.setNamesDotted <- function(object, rownames = FALSE) {
    if (.checkNames(object)) {
        object <- set_names(object, .makeNamesDotted(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- set_rownames(object, .makeNamesDotted(rownames(object)))
    }
    object
}

#' @rdname names
#' @export
setMethod("dotted", "ANY", .setNamesDotted)

#' @rdname names
#' @export
setMethod("dotted", "character", function(object) {
    if (isTRUE(.checkNames(object))) {
        .setNamesDotted(object, rownames = FALSE)
    } else {
        .makeNamesDotted(object)
    }
})

#' @rdname names
#' @export
setMethod("dotted", "data.frame", .setNamesDotted)

#' @rdname names
#' @export
setMethod("dotted", "list", function(object) {
    .setNamesDotted(object, rownames = FALSE)
})

#' @rdname names
#' @export
setMethod("dotted", "matrix", .setNamesDotted)

#' @rdname names
#' @export
setMethod("dotted", "tbl_df", function(object) {
    .setNamesDotted(object, rownames = FALSE)
})





# camelCase ====
#' @rdname names
#' @usage NULL
.makeNamesCamel <- function(object) {
    object %>%
        .makeNamesDotted %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}

#' @rdname names
#' @usage NULL
.setNamesCamel <- function(object, rownames = FALSE) {
    if (.checkNames(object)) {
        object <- set_names(object, .makeNamesCamel(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- set_rownames(object, .makeNamesCamel(rownames(object)))
    }
    object
}

#' @rdname names
#' @export
setMethod("camel", "ANY", .setNamesCamel)

#' @rdname names
#' @export
setMethod("camel", "character", function(object) {
    if (isTRUE(.checkNames(object))) {
        .setNamesCamel(object, rownames = FALSE)
    } else {
        .makeNamesCamel(object)
    }
})

#' @rdname names
#' @export
setMethod("camel", "data.frame", .setNamesCamel)

#' @rdname names
#' @export
setMethod("camel", "list", function(object) {
    .setNamesCamel(object, rownames = FALSE)
})

#' @rdname names
#' @export
setMethod("camel", "matrix", .setNamesCamel)

#' @rdname names
#' @export
setMethod("camel", "tbl_df", function(object) {
    .setNamesCamel(object, rownames = FALSE)
})



# snake_case ====
#' @rdname names
#' @usage NULL
.makeNamesSnake <- function(object) {
    object %>%
        .makeNamesDotted %>%
        str_replace_all("\\.", "_")
}

#' @rdname names
#' @usage NULL
.setNamesSnake <- function(object, rownames = FALSE) {
    if (.checkNames(object)) {
        object <- set_names(object, .makeNamesSnake(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- set_rownames(object, .makeNamesSnake(rownames(object)))
    }
    object
}

#' @rdname names
#' @export
setMethod("snake", "ANY", .setNamesSnake)

#' @rdname names
#' @export
setMethod("snake", "character", function(object) {
    if (isTRUE(.checkNames(object))) {
        .setNamesSnake(object, rownames = FALSE)
    } else {
        .makeNamesSnake(object)
    }
})

#' @rdname names
#' @export
setMethod("snake", "data.frame", .setNamesSnake)

#' @rdname names
#' @export
setMethod("snake", "list", function(object) {
    .setNamesSnake(object, rownames = FALSE)
})

#' @rdname names
#' @export
setMethod("snake", "matrix", .setNamesSnake)

#' @rdname names
#' @export
setMethod("snake", "tbl_df", function(object) {
    .setNamesSnake(object, rownames = FALSE)
})



# titleCase ====
#' @rdname names
#' @usage NULL
.makeTitleCase <- function(object) {
    object %>%
        gsub("\\b([a-z])", "\\U\\1", ., perl = TRUE)
}

#' @rdname names
#' @export
setMethod("titleCase", "character", .makeTitleCase)



# firstCase ====
#' @rdname names
#' @usage NULL
.makeFirstCase <- function(object) {
    object %>%
        .makeTitleCase %>%
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE)
}

#' @rdname names
#' @export
setMethod("firstCase", "character", .makeFirstCase)



# Name variants ====
#' @rdname names
#' @export
sanitizeNames <- snake

#' @rdname names
#' @export
sanitizeNames -> sanitiseNames
