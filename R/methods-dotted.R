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
#'     package = "basejump"
#' ))
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



.dotted <- function(object) {
    object %>%
        as.character() %>%
        make.names(unique = FALSE, allow_ = FALSE) %>%
        # Convert non-alphanumeric characters to dots
        gsub("[^[:alnum:]]", ".", .) %>%
        # Combine multiple dots
        gsub("[\\.]+", ".", .) %>%
        # Strip leading or trailing dots
        gsub("(^\\.|\\.$)", "", .) %>%
        # Coerce `"NA"` back to `NA` after `make.names()`
        fixNA() %>%
        .sanitizeAcronyms() %>%
        # Establish word boundaries for camelCase acronyms
        # (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`)
        # Acronym following a word
        gsub("([a-z])([A-Z])", "\\1.\\2", .) %>%
        # Word following an acronym
        gsub("([A-Z0-9])([A-Z])([a-z])", "\\1.\\2\\3", .)
}



.dotted.dim <- function(object, rownames = FALSE) {
    if (!is.logical(rownames)) {
        abort("`rownames` must be logical")
    }
    if (!is.null(dimnames(object))) {
        # Colnames
        if (!is.null(colnames(object))) {
            colnames(object) <- .dotted(colnames(object))
        }
        # Rownames
        if (isTRUE(rownames) & .checkRownames(object)) {
            rownames(object) <- .dotted(rownames(object))
        }
    } else if (!is.null(names(object))) {
        names(object) <- .dotted(names(object))
    }
    object
}



.dotted.names <- function(object) {
    .dotted.dim(object, rownames = FALSE)
}



.dotted.vector <- function(object) {  # no lint
    if (!is.null(names(object))) {
        names <- .dotted(names(object))
    } else {
        names <- NULL
    }
    object <- .dotted(object)
    names(object) <- names
    object
}



.sanitizeAcronyms <- function(object) {
    object %>%
        # Ensure identifier is "ID"
        gsub("\\b(id)\\b", "ID", ., ignore.case = TRUE) %>%
        # Sanitize mixed case scientific acronyms
        gsub("\\b(mRNA)\\b", "MRNA", .) %>%
        gsub("\\b(miRNA)\\b", "MIRNA", .) %>%
        gsub("\\b(ncRNA)\\b", "NCRNA", .) %>%
        gsub("\\b(piRNA)\\b", "PIRNA", .) %>%
        gsub("\\b(rRNA)\\b", "RRNA", .) %>%
        gsub("\\b(RNAi)\\b", "RNAI", .)
}



# Methods ======================================================================
#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("ANY"),
    .dotted.dim)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("character"),
    .dotted.vector)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("data.frame"),
    .dotted.dim)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("DataFrame"),
    .dotted.dim)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("factor"),
    .dotted.vector)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("list"),
    .dotted.names)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("matrix"),
    .dotted.dim)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("tbl_df"),
    .dotted.names)
