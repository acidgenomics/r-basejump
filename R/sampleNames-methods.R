#' Sample Names
#'
#' This function will return the human readable sample names if defined
#' in the `sampleName` column of `sampleData()`. Otherwise it will return
#' the syntactically valid names defined as the rownames of `sampleData()`.
#'
#' @name sampleNames
#' @aliases sampleNames<-
#'
#' @inheritParams params
#'
#' @return Named `character` vector of the sample names.
#'
#' @examples
#' data(rse)
#' object <- rse
#' x <- sampleNames(object)
#' print(x)
#'
#' ## Assignment support.
#' value <- sampleNames(object)
#' value <- toupper(value)
#' print(value)
#' sampleNames(object) <- value
#' x <- sampleNames(object)
#' print(x)
NULL



#' @importFrom Biobase sampleNames
#' @aliases NULL
#' @export
Biobase::sampleNames

#' @importFrom Biobase sampleNames<-
#' @aliases NULL
#' @export
Biobase::`sampleNames<-`



sampleNames.SummarizedExperiment <-  # nolint
    function(object) {
        data <- sampleData(object)
        assertSubset("sampleName", colnames(data))
        out <- as.character(data[["sampleName"]])
        names(out) <- rownames(data)
        out
    }



#' @rdname sampleData
#' @export
setMethod(
    f = "sampleNames",
    signature = signature("SummarizedExperiment"),
    definition = sampleNames.SummarizedExperiment
)



`sampleNames<-.SummarizedExperiment` <-  # nolint
    function(object, value) {
        assertHasNames(value)
        # Note that these will correspond to columns for bulk RNA-seq but not
        # single-cell RNA-seq samples, which map to cells.
        ids <- names(sampleNames(object))
        assertHasLength(ids)
        # Require the input to match the original IDs.
        assertSetEqual(names(value), ids)
        # Now safe to reorder the value vector to match.
        value <- value[ids]
        # Check that the slotting destination matches.
        assertIdentical(names(value), rownames(sampleData(object)))
        sampleData(object)[["sampleName"]] <- value
        validObject(object)
        object
    }



#' @rdname sampleNames
#' @export
setMethod(
    f = "sampleNames<-",
    signature = signature(
        object = "SummarizedExperiment",
        value = "character"
    ),
    definition = `sampleNames<-.SummarizedExperiment`
)
