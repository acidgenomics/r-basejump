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
#' @family Name Functions
#'
#' @inheritParams general
#'
#' @param object Character vector or an object for which [names()] assignment
#'   will be meaningful.
#' @param rownames Apply sanitization on row names. This is not recommended
#'   by default, since rownames commonly contain gene identifiers that should
#'   not be modified.
#' @param colnames Apply sanitization on column names. This is generally
#'   recommended by default.
#'
#' @return Object with syntatically valid names. For objects supporting
#'   [base::names()], the underlying data returns unchanged.
#'
#' @examples
#' load(system.file("extdata/makeNames.rda", package = "basejump"))
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
#' dotted(dataFrame, rownames = TRUE)
#' dotted(dataFrame, rownames = FALSE)
#'
#' # Named list
#' list <- makeNames$list
#' print(list)
#' dotted(list)
NULL



# Constructors =================================================================
.dotted <- function(object) {
    assert_is_any_of(object, c("character", "factor"))
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



.dotted.ANY <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE
) {
    # Passthrough: rownames, colnames
    if (!is.null(dimnames(object))) {
        .dotted.dim(object, rownames = rownames, colnames = colnames)
    } else if (!is.null(names(object))) {
        .dotted.names(object)
    } else {
        object
    }
}



.dotted.dim <- function(  # nolint
    object,
    rownames = FALSE,
    colnames = TRUE
) {
    assert_has_dimnames(object)
    assert_is_a_bool(rownames)
    if (isTRUE(rownames) && hasRownames(object)) {
        rownames(object) <- .dotted(rownames(object))
    }
    if (isTRUE(colnames) && has_colnames(object)) {
        colnames(object) <- .dotted(colnames(object))
    }
    object
}



.dotted.mcols <- function(object) {  # nolint
    colnames <- colnames(mcols(object))
    colnames <- dotted(colnames)
    colnames(mcols(object)) <- colnames
    object
}



.dotted.names <- function(object) {  # nolint
    assert_has_names(object)
    names(object) <- .dotted(names(object))
    object
}



.dotted.tibble <- function(object) {  # nolint
    .dotted.dim(object, rownames = FALSE, colnames = TRUE)
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
    assert_is_character(object)
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
    .dotted.ANY
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("character"),
    .dotted.vector
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("data.frame"),
    .dotted.dim
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("DataFrame"),
    .dotted.dim
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("factor"),
    .dotted.vector
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("GRanges"),
    .dotted.mcols
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("list"),
    .dotted.names
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("List"),
    .dotted.names
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("matrix"),
    .dotted.dim
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("SimpleList"),
    .dotted.names
)



#' @rdname dotted
#' @export
setMethod(
    "dotted",
    signature("tbl_df"),
    .dotted.tibble
)
