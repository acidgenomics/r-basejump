#' Organism
#' @name organism
#' @inheritParams params
#' @seealso `detectOrganism()`.
#' @examples
#' data(rse)
#' organism(rse)
NULL



#' @importFrom BiocGenerics organism
#' @aliases NULL
#' @export
BiocGenerics::organism



organism.matrix <-  # nolint
    function(object) {
        # Assume gene identifiers are defined in the rownames.
        assertHasRownames(object)
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



organism.DataFrame <-  # nolint
    organism.data.frame



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("DataFrame"),
    definition = organism.DataFrame
)



organism.GRanges <-  # nolint
    function(object) {
        assertHasNames(object)
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
        organism <- metadata(object)[["organism"]]
        if (is_a_string(organism)) {
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
