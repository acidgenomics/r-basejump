#' Organism
#'
#' @name organism
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return `character(1)`.
#' Latin organism name (e.g. *Homo sapiens*).
#'
#' @seealso `detectOrganism()`.
#'
#' @examples
#' data(rse, package = "acidtest")
#' organism(rse)
NULL



# Assuming gene identifiers are defined in the rownames.
organism.matrix <-  # nolint
    function(object) {
        assert(hasRownames(object))
        detectOrganism(rownames(object))
    }



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("matrix"),
    definition = organism.matrix
)



organism.sparseMatrix <-  # nolint
    organism.matrix



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("sparseMatrix"),
    definition = organism.sparseMatrix
)



organism.data.frame <-  # nolint
    organism.matrix



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("data.frame"),
    definition = organism.data.frame
)



# Note that DataFrame and GRanges inherit from this class.
organism.Annotated <-  # nolint
    function(object) {
        metadata(object)[["organism"]]
    }



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("Annotated"),
    definition = organism.Annotated
)



organism.DataTable <-  # nolint
    function(object) {
        # Attempt to use metadata stash, if defined.
        organism <- organism.Annotated(object)
        if (isString(organism)) {
            return(organism)
        }
        # Otherwise, fall back to matrix method.
        organism.matrix(object)
    }



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("DataTable"),
    definition = organism.DataTable
)



organism.GRanges <-  # nolint
    function(object) {
        # Attempt to use metadata stash, if defined.
        organism <- organism.Annotated(object)
        if (isString(organism)) {
            return(organism)
        }
        assert(hasNames(object))
        detectOrganism(names(object))
    }



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("GRanges"),
    definition = organism.GRanges
)



organism.SummarizedExperiment <-  # nolint
    function(object) {
        # Attempt to use metadata stash, if defined.
        organism <- organism.Annotated(object)
        if (isString(organism)) {
            return(organism)
        }

        # Fall back to detecting from rowRanges or rownames.
        if ("geneID" %in% colnames(rowData(object))) {
            x <- as.character(rowData(object)[["geneID"]])
        } else {
            x <- rownames(object)
        }

        detectOrganism(x)
    }



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("SummarizedExperiment"),
    definition = organism.SummarizedExperiment
)
