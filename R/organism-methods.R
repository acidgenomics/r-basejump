#' Organism Accessor
#'
#' @name organism
#' @family Data Functions
#' @author Michael Steinbaugh
#' @importFrom BiocGenerics organism
#' @export
#'
#' @inheritParams general
#'
#' @seealso [detectOrganism()].
#'
#' @examples
#' data(rse_small)
#' organism(rse_small)
NULL



.organism.matrix <-  # nolint
    function(object) {
        # Assume gene identifiers are defined in the rownames.
        assertHasRownames(object)
        detectOrganism(rownames(object))
    }



.organism.GRanges <-  # nolint
    function(object) {
        assert_has_names(object)
        detectOrganism(names(object))
    }




# Then attempt to check rowData.
# Finally, check against the rownames.
.organism.SE <-  # nolint
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
    signature = signature("matrix"),
    definition = .organism.matrix
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("sparseMatrix"),
    definition = getMethod("organism", "matrix")
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("data.frame"),
    definition = getMethod("organism", "matrix")
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("DataFrame"),
    definition = getMethod("organism", "matrix")
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("GRanges"),
    definition = .organism.GRanges
)



#' @rdname organism
#' @export
setMethod(
    f = "organism",
    signature = signature("SummarizedExperiment"),
    definition = .organism.SE
)
