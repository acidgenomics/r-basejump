#' @name camelCase
#' @inherit syntactic::camelCase
#' @note Updated 2021-01-21.
#'
#' @inheritParams AcidRoxygen::params
#' @param assayNames `logical(1)`.
#'   Sanitize assay names.
#' @param colData `logical(1)`.
#'   Sanitize column names of column data.
#' @param mcols `logical(1)`.
#'   Sanitize names of metadata columns (i.e. `DataFrame`).
#' @param metadata `logical(1)`.
#'   Sanitize metadata names.
#' @param rowData `logical(1)`.
#'   Sanitize the row data names.
#' @param rownames `logical(1)`.
#'   Apply sanitization on row names. This is not generally recommended by
#'   default, since rownames commonly contain gene identifiers that should not
#'   be modified.
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' lapply(syntactic, camelCase)
NULL



`camelCase,atomic` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- camelCase(names(object), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("atomic"),
    definition = `camelCase,atomic`
)



`camelCase,factor` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- camelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- camelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- names
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("factor"),
    definition = `camelCase,factor`
)



`camelCase,list` <- `camelCase,atomic`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("list"),
    definition = `camelCase,list`
)



`camelCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("matrix"),
    definition = `camelCase,matrix`
)



`camelCase,data.frame` <- `camelCase,matrix`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("data.frame"),
    definition = `camelCase,data.frame`
)



`camelCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <-
                camelCase(names(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                camelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Vector"),
    definition = `camelCase,Vector`
)



`camelCase,DataFrame` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                camelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object

    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("DataFrame"),
    definition = `camelCase,DataFrame`
)



`camelCase,Ranges` <- `camelCase,Vector`  # nolint
formals(`camelCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Ranges"),
    definition = `camelCase,Ranges`
)



`camelCase,Matrix` <- `camelCase,matrix`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Matrix"),
    definition = `camelCase,Matrix`
)



`camelCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE,
        strict = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <-
                camelCase(names(assays(object)), strict = strict)
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                camelCase(colnames(rowData(object)), strict = strict)
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <-
                camelCase(colnames(colData(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                camelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("SummarizedExperiment"),
    definition = `camelCase,SummarizedExperiment`
)
