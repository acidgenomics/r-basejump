#' Dotted Case
#'
#' @details For unnamed character vectors, this function will sanitize the
#' underlying values. Otherwise, it will set [names()] and/or [rownames()] on
#' objects supporting name assignments. They return the object without
#' modification of the underlying data.
#'
#' @note `dotted.case` support is provided for matching against base R
#'   parameters, but we strongly advise against using it for object and/or
#'   argument name assignments.
#'
#' @rdname dotted
#' @name dotted
#' @family Make Names Utilities
#'
#' @inheritParams AllGenerics
#'
#' @param object Character vector or an object for which [names()] assignment
#'   will be meaningful.
#' @param rownames Apply sanitization on row names. This is not recommended
#'   by default, since rownames commonly contain gene identifiers that should
#'   not be modified.
#'
#' @return Object with syntatically valid names. For objects supporting
#'   [base::names()], the underlying data returns unchanged.
#'
#' @examples
#' load(system.file(
#'     file.path("extdata", "makeNames.rda"),
#'     package = "basejump"))
#'
#' # Character vector
#' character <- makeNames$character
#' print(character)
#' dotted(character)
#'
#' # Named character vector
#' namedCharacter <- makeNames$namedCharacter
#' dotted(namedCharacter)
#'
#' # Factor
#' factor <- makeNames$factor
#' print(factor)
#' dotted(factor)
#'
#' # data.frame
#' dataFrame <- makeNames$dataFrame
#' print(dataFrame)
#' dotted(dataFrame, rownames = FALSE)
#' dotted(dataFrame, rownames = TRUE)
#'
#' # Named list
#' list <- makeNames$list
#' print(list)
#' dotted(list)
NULL



# Constructors =================================================================
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
        if (!identical(
            rownames(object),
            as.character(seq_len(nrow(object)))
        )) {
            TRUE
        } else {
            FALSE
        }
    } else {
        FALSE
    }
}



.sanitizeAcronyms <- function(object) {
    object %>%
        # Ensure identifier is "ID"
        gsub(
            pattern = "\\b(id)\\b",
            replacement = "ID",
            x = .,
            ignore.case = TRUE
        ) %>%
        # Sanitize mixed case scientific acronyms
        gsub(
            pattern = "\\b(mRNA)\\b",
            replacement = "MRNA",
            x = .
        ) %>%
        gsub(
            pattern = "\\b(miRNA)\\b",
            replacement = "MIRNA",
            x = .
        ) %>%
        gsub(
            pattern = "\\b(ncRNA)\\b",
            replacement = "NCRNA",
            x = .
        ) %>%
        gsub(
            pattern = "\\b(piRNA)\\b",
            replacement = "PIRNA",
            x = .
        ) %>%
        gsub(
            pattern = "\\b(rRNA)\\b",
            replacement = "RRNA",
            x = .
        ) %>%
        gsub(
            pattern = "\\b(RNAi)\\b",
            replacement = "RNAI",
            x = .
        )
}



.makeNamesDotted <- function(object) {
    object %>%
        as.character() %>%
        make.names(unique = FALSE, allow_ = FALSE) %>%
        # Convert non-alphanumeric characters to dots
        gsub(
            pattern = "[^[:alnum:]]",
            replacement = ".",
            x = .
        ) %>%
        # Combine multiple dots
        gsub(
            pattern = "[\\.]+",
            replacement = ".",
            x = .
        ) %>%
        # Strip leading or trailing dots
        gsub(
            pattern = "(^\\.|\\.$)",
            replacement = "",
            x = .
        ) %>%
        # Coerce `"NA"` back to `NA` after `make.names()`
        fixNA() %>%
        .sanitizeAcronyms() %>%
        # Establish word boundaries for camelCase acronyms
        # (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`)
        # Acronym following a word
        gsub(
            pattern = "([a-z])([A-Z])",
            replacement = "\\1.\\2",
            x = .
        ) %>%
        # Word following an acronym
        gsub(
            pattern = "([A-Z0-9])([A-Z])([a-z])",
            replacement = "\\1.\\2\\3",
            x = .
        )
}



.setNamesDotted <- function(
    object,
    rownames = FALSE) {
    if (.checkNames(object)) {
        object <- setNames(object, .makeNamesDotted(names(object)))
    }
    if (isTRUE(rownames) & .checkRownames(object)) {
        rownames(object) <- .makeNamesDotted(rownames(object))
    }
    object
}



.setNamesDottedNoRownames <- function(object) {
    .setNamesDotted(object, rownames = FALSE)
}



.dottedVector <- function(object) {
    if (isTRUE(.checkNames(object))) {
        names <- .makeNamesDotted(names(object))
    } else {
        names <- NULL
    }
    object <- .makeNamesDotted(object)
    names(object) <- names
    object
}



# Methods ======================================================================
#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("ANY"),
    .setNamesDotted)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("character"),
    .dottedVector)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("data.frame"),
    .setNamesDotted)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("DataFrame"),
    .setNamesDotted)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("factor"),
    .dottedVector)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("list"),
    .setNamesDottedNoRownames)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("matrix"),
    .setNamesDotted)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("tbl_df"),
    .setNamesDottedNoRownames)
