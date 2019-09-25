#' @name dottedCase
#' @inherit syntactic::dottedCase
#' @inheritParams camelCase
#' @note Updated 2019-09-25.
#' @examples
#' data(syntactic, package = "acidtest")
#' lapply(syntactic, dottedCase)
NULL



#' @rdname dottedCase
#' @name dottedCase
#' @importFrom syntactic dottedCase
#' @usage dottedCase(object, ...)
#' @export
NULL



`dottedCase,atomic` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        object
    }



#' @rdname dottedCase
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



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("factor"),
    definition = `dottedCase,factor`
)



`dottedCase,list` <- `dottedCase,atomic`  # nolint



#' @rdname dottedCase
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



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("matrix"),
    definition = `dottedCase,matrix`
)



`dottedCase,data.frame` <- `dottedCase,matrix`  # nolint



#' @rdname dottedCase
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



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Vector"),
    definition = `dottedCase,Vector`
)



`dottedCase,DataTable` <-  # nolint
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



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("DataTable"),
    definition = `dottedCase,DataTable`
)



`dottedCase,Ranges` <- `dottedCase,Vector`  # nolint
formals(`dottedCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Ranges"),
    definition = `dottedCase,Ranges`
)



`dottedCase,Matrix` <- `dottedCase,matrix`  # nolint



#' @rdname dottedCase
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



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("SummarizedExperiment"),
    definition = `dottedCase,SummarizedExperiment`
)
