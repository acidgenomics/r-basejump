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
#' loadRemoteData("http://basejump.seq.cloud/makeNames.rda")
#'
#' # Character vector
#' print(makeNames$vec)
#' dotted(makeNames$vec)
#'
#' # data.frame
#' print(makeNames$df)
#' dotted(makeNames$df, rownames = TRUE)
NULL



# Constructors ====
.sanitizeAcronyms <- function(object) {
    object %>%
        # Ensure identifier is "ID"
        gsub(x = .,
             pattern = "\\b(id)\\b",
             replacement = "ID",
             ignore.case = TRUE) %>%
        # Sanitize mixed case scientific acronyms
        gsub(x = .,
             pattern = "\\b(mRNA)\\b",
             replacement = "MRNA") %>%
        gsub(x = .,
             pattern = "\\b(miRNA)\\b",
             replacement = "MIRNA") %>%
        gsub(x = .,
             pattern = "\\b(ncRNA)\\b",
             replacement = "NCRNA") %>%
        gsub(x = .,
             pattern = "\\b(piRNA)\\b",
             replacement = "PIRNA") %>%
        gsub(x = .,
             pattern = "\\b(rRNA)\\b",
             replacement = "RRNA") %>%
        gsub(x = .,
             pattern = "\\b(RNAi)\\b",
             replacement = "RNAI")
}



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



.makeNamesDotted <- function(object) {
    object %>%
        as.character() %>%
        make.names(unique = FALSE, allow_ = FALSE) %>%
        # Convert non-alphanumeric characters to dots
        gsub(x = .,
             pattern = "[^[:alnum:]]",
             replacement = ".") %>%
        # Combine multiple dots
        gsub(x = .,
             pattern = "[\\.]+",
             replacement = ".") %>%
        # Strip leading or trailing dots
        gsub(x = .,
             pattern = "(^\\.|\\.$)",
             replacement = "") %>%
        # Coerce `"NA"` back to `NA` after `make.names()`
        fixNA() %>%
        .sanitizeAcronyms() %>%
        # Establish word boundaries for camelCase acronyms
        # (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`)
        # Acronym following a word
        gsub(x = .,
             pattern = "([a-z])([A-Z])",
             replacement = "\\1.\\2") %>%
        # Word following an acronym
        gsub(x = .,
             pattern = "([A-Z0-9])([A-Z])([a-z])",
             replacement = "\\1.\\2\\3")
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



.dottedVector <- function(object) {
    if (isTRUE(.checkNames(object))) {
        names <- .makeNamesDotted(names(object))
    }
    object <- .makeNamesDotted(object)
    names(object) <- names
    object
}



# Methods ====
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
    function(object) {
        .setNamesDotted(object, rownames = FALSE)
    })



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
    function(object) {
        .setNamesDotted(object, rownames = FALSE)
    })
