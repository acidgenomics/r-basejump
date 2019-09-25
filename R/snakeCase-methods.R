#' @name snakeCase
#' @inherit syntactic::snakeCase
#' @note Updated 2019-09-25.
#' @examples
#' data(syntactic, package = "acidtest")
#' lapply(syntactic, snakeCase)
NULL



`snakeCase,atomic` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- snakeCase(names(object))
        }
        object
    }



#' @rdname snakeCase
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



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("factor"),
    definition = `snakeCase,factor`
)



`snakeCase,list` <- `snakeCase,atomic`  # nolint



#' @rdname snakeCase
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



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("matrix"),
    definition = `snakeCase,matrix`
)



`snakeCase,data.frame` <- `snakeCase,matrix`  # nolint



#' @rdname snakeCase
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



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Vector"),
    definition = `snakeCase,Vector`
)



`snakeCase,DataTable` <-  # nolint
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



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("DataTable"),
    definition = `snakeCase,DataTable`
)



`snakeCase,Ranges` <- `snakeCase,Vector`  # nolint
formals(`snakeCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Ranges"),
    definition = `snakeCase,Ranges`
)



`snakeCase,Matrix` <- `snakeCase,matrix`  # nolint



#' @rdname snakeCase
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



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("SummarizedExperiment"),
    definition = `snakeCase,SummarizedExperiment`
)
