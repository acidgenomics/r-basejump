#' Make Syntactically Valid Names
#'
#' These are convenience functions that sanitize names into `camelCase`
#' (**preferred**), `snake_case`, or `dotted.case`.
#'
#' For unnamed character vectors, these functions will sanitize the underlying
#' values. Otherwise, the functions will set [names()] and/or [rownames()] on
#' objects supporting name assignments. They return the object without
#' modification of the underlying data.
#'
#' @rdname makeNames
#' @name makeNames
#'
#' @note `dotted.case` support is provided for matching against base R
#'   parameters, but we strongly advise against using it for object and/or
#'   argument name assignments.
#'
#' @param object Character vector or an object for which [names()] assignment
#'   will be meaningful.
#' @param rownames Apply sanitization on row names. This is not recommended
#'   by default, since rownames commonly contain gene identifiers that should
#'   not be modified.
#' @param strict Enforce strict rules for name sanitization. **Recommended**
#'   and generally should not be set to `FALSE`.
#'
#' @return Object with syntatically valid names. For objects supporting
#'   [base::names()], the underlying data returns unchanged.
#'
#' @examples
#' # Unnamed character vector
#' unnamedVec <-
#'   c("hello world",
#'     "HELLO WORLD",
#'     "RNAi clones",
#'     "worfdbHTMLRemap",
#'     123,
#'     NA)
#'
#' # Named character vector
#' namedVec <- c(Item.A = "hello world", Item.B = "HELLO WORLD")
#'
#' # Data frame with colnames and rownames
#' df <- head(mtcars)
#'
#' # Tibble, with colnames. Rownames aren't supported on these!
#' tbl <- head(starwars)
#'
#' # List, with assigned names
#' lst <- list(Item.A = c(1, 2),
#'             Item.B = c(3, 4))
#'
#'
#' # camelCase
#' camel(unnamedVec)
#' camel(namedVec)
#' camel(df)
#' camel(df, rownames = TRUE)
#' camel(tbl)
#' camel(lst)
#'
#'
#' # snake_case
#' snake(unnamedVec)
#' snake(namedVec)
#' snake(df)
#' snake(df, rownames = TRUE)
#' snake(tbl)
#' snake(lst)
#'
#'
#' # dotted.case
#' dotted(unnamedVec)
#' dotted(namedVec)
#' dotted(df)
#' dotted(df, rownames = TRUE)
#' dotted(tbl)
#' dotted(lst)
NULL



# General Constructors ====
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
.makeNamesDotted <- function(object, strict = TRUE) {
    x <- object %>%
        as.character %>%
        make.names %>%
        # Convert non-alphanumeric characters to dots
        str_replace_all("[^[:alnum:]]", ".") %>%
        # Combine multiple dots
        str_replace_all("[\\.]+", ".") %>%
        # Strip leading or trailing dots
        str_replace_all("(^\\.|\\.$)", "")
    if (isTRUE(strict)) {
        x %>%
            # Special word exceptions
            str_replace_all("RNAi", "Rnai") %>%
            # Handle snakeCase acronyms
            # (e.g. `worfdbHTMLRemap` -> `worfdb.html.remap`)
            gsub("([A-Z])([A-Z0-9]+)([A-Z])([a-z])",
                 "\\1\\L\\2\\U\\3\\L\\4", ., perl = TRUE) %>%
            # Convert remaining acronyms to mixed case
            gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE) %>%
            # Make first letter lowercase
            gsub("(^[A-Z]{1})", "\\L\\1", ., perl = TRUE) %>%
            # Convert camelCase
            gsub("([a-z0-9])([A-Z])", "\\1.\\L\\2", ., perl = TRUE) %>%
            tolower
    } else {
        x
    }
}



.setNamesDotted <- function(object, rownames = FALSE, strict = TRUE) {
    if (.checkNames(object)) {
        object <- setNames(
            object,
            .makeNamesDotted(names(object),
                             strict = strict))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        object <- set_rownames(
            object,
            .makeNamesDotted(rownames(object),
                             strict = strict))
    }
    object
}



#' @rdname makeNames
#' @export
setMethod("dotted", "ANY", .setNamesDotted)



#' @rdname makeNames
#' @export
setMethod("dotted", "character", function(object, strict = TRUE) {
    if (isTRUE(.checkNames(object))) {
        .setNamesDotted(object,
                        strict = strict,
                        rownames = FALSE)
    } else {
        .makeNamesDotted(object,
                         strict = strict)
    }
})



#' @rdname makeNames
#' @export
setMethod("dotted", "data.frame", .setNamesDotted)



#' @rdname makeNames
#' @export
setMethod("dotted", "list", function(object, strict = TRUE) {
    .setNamesDotted(object,
                    strict = strict,
                    rownames = FALSE)
})



#' @rdname makeNames
#' @export
setMethod("dotted", "matrix", .setNamesDotted)



#' @rdname makeNames
#' @export
setMethod("dotted", "tbl_df", function(object, strict = TRUE) {
    .setNamesDotted(object,
                    strict = strict,
                    rownames = FALSE)
})
