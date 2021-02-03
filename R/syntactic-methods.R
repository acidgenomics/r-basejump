#' Syntactic naming functions
#'
#' @name syntactic
#' @note Updated 2021-02-03.
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



#' @rdname syntactic
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



#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("factor"),
    definition = `camelCase,factor`
)



`camelCase,list` <- `camelCase,atomic`  # nolint



#' @rdname syntactic
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



#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("matrix"),
    definition = `camelCase,matrix`
)



`camelCase,data.frame` <- `camelCase,matrix`  # nolint



#' @rdname syntactic
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



#' @rdname syntactic
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



#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("DataFrame"),
    definition = `camelCase,DataFrame`
)



`camelCase,Ranges` <- `camelCase,Vector`  # nolint
formals(`camelCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Ranges"),
    definition = `camelCase,Ranges`
)



`camelCase,Matrix` <- `camelCase,matrix`  # nolint



#' @rdname syntactic
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



#' @rdname syntactic
#' @export
setMethod(
    f = "camelCase",
    signature = signature("SummarizedExperiment"),
    definition = `camelCase,SummarizedExperiment`
)



`dottedCase,atomic` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("atomic"),
    definition = `dottedCase,atomic`
)



`dottedCase,factor` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- dottedCase(names(object))
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- dottedCase(object)
        object <- as.factor(object)
        names(object) <- names
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("factor"),
    definition = `dottedCase,factor`
)



`dottedCase,list` <- `dottedCase,atomic`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("list"),
    definition = `dottedCase,list`
)



`dottedCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("matrix"),
    definition = `dottedCase,matrix`
)



`dottedCase,data.frame` <- `dottedCase,matrix`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("data.frame"),
    definition = `dottedCase,data.frame`
)



`dottedCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- dottedCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Vector"),
    definition = `dottedCase,Vector`
)



`dottedCase,DataFrame` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- dottedCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object

    }



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("DataFrame"),
    definition = `dottedCase,DataFrame`
)



`dottedCase,Ranges` <- `dottedCase,Vector`  # nolint
formals(`dottedCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Ranges"),
    definition = `dottedCase,Ranges`
)



`dottedCase,Matrix` <- `dottedCase,matrix`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Matrix"),
    definition = `dottedCase,Matrix`
)



`dottedCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <- dottedCase(names(assays(object)))
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <- dottedCase(colnames(rowData(object)))
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <- dottedCase(colnames(colData(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- dottedCase(names(metadata(object)))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("SummarizedExperiment"),
    definition = `dottedCase,SummarizedExperiment`
)



`snakeCase,atomic` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- snakeCase(names(object))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("atomic"),
    definition = `snakeCase,atomic`
)



`snakeCase,factor` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- snakeCase(names(object))
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- snakeCase(object)
        object <- as.factor(object)
        names(object) <- names
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("factor"),
    definition = `snakeCase,factor`
)



`snakeCase,list` <- `snakeCase,atomic`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("list"),
    definition = `snakeCase,list`
)



`snakeCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snakeCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snakeCase(colnames(object))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("matrix"),
    definition = `snakeCase,matrix`
)



`snakeCase,data.frame` <- `snakeCase,matrix`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("data.frame"),
    definition = `snakeCase,data.frame`
)



`snakeCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- snakeCase(names(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- snakeCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- snakeCase(names(metadata(object)))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Vector"),
    definition = `snakeCase,Vector`
)



`snakeCase,DataFrame` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snakeCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snakeCase(colnames(object))
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <- snakeCase(names(mcols(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- snakeCase(names(metadata(object)))
        }
        object

    }



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("DataFrame"),
    definition = `snakeCase,DataFrame`
)



`snakeCase,Ranges` <- `snakeCase,Vector`  # nolint
formals(`snakeCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Ranges"),
    definition = `snakeCase,Ranges`
)



`snakeCase,Matrix` <- `snakeCase,matrix`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Matrix"),
    definition = `snakeCase,Matrix`
)



`snakeCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE
    ) {
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snakeCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snakeCase(colnames(object))
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <- snakeCase(names(assays(object)))
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <- snakeCase(colnames(rowData(object)))
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <- snakeCase(colnames(colData(object)))
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <- snakeCase(names(metadata(object)))
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("SummarizedExperiment"),
    definition = `snakeCase,SummarizedExperiment`
)



`upperCamelCase,atomic` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- upperCamelCase(names(object), strict = strict)
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("atomic"),
    definition = `upperCamelCase,atomic`
)



`upperCamelCase,factor` <-  # nolint
    function(object, names = TRUE, strict = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- upperCamelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- upperCamelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- names
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("factor"),
    definition = `upperCamelCase,factor`
)



`upperCamelCase,list` <- `upperCamelCase,atomic`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("list"),
    definition = `upperCamelCase,list`
)



`upperCamelCase,matrix` <-  # nolint
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
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("matrix"),
    definition = `upperCamelCase,matrix`
)



`upperCamelCase,data.frame` <- `upperCamelCase,matrix`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("data.frame"),
    definition = `upperCamelCase,data.frame`
)



`upperCamelCase,Vector` <-  # nolint
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
                upperCamelCase(names(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                upperCamelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Vector"),
    definition = `upperCamelCase,Vector`
)



`upperCamelCase,DataFrame` <-  # nolint
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
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            names(mcols(object)) <-
                upperCamelCase(names(mcols(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object

    }



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("DataFrame"),
    definition = `upperCamelCase,DataFrame`
)



`upperCamelCase,Ranges` <- `upperCamelCase,Vector`  # nolint
formals(`upperCamelCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Ranges"),
    definition = `upperCamelCase,Ranges`
)



`upperCamelCase,Matrix` <- `upperCamelCase,matrix`  # nolint



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Matrix"),
    definition = `upperCamelCase,Matrix`
)



`upperCamelCase,SummarizedExperiment` <-  # nolint
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
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <-
                upperCamelCase(names(assays(object)), strict = strict)
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                upperCamelCase(colnames(rowData(object)), strict = strict)
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <-
                upperCamelCase(colnames(colData(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname syntactic
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("SummarizedExperiment"),
    definition = `upperCamelCase,SummarizedExperiment`
)
