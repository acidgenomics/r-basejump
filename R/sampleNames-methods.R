#' Sample Names
#'
#' This function will return the human readable sample names if defined
#' in the `sampleName` column of [sampleData()]. Otherwise it will return
#' the syntactically valid names defined as the rownames of [sampleData()].
#'
#' @name sampleNames
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @importFrom Biobase sampleNames
#'
#' @inheritParams general
#'
#' @return Named `character` vector of the sample names.
#'
#' @examples
#' # SummarizedExperiment ====
#' sampleNames(rse_bcb)
#' sampleNames(rse_dds)
NULL



#' @rdname sampleData
#' @export
setMethod(
    "sampleNames",
    signature("SummarizedExperiment"),
    function(object) {
        data <- sampleData(object)
        assert_is_subset("sampleName", colnames(data))
        data <- data[sort(rownames(data)), , drop = FALSE]
        vec <- as.character(data[, "sampleName", drop = TRUE])
        names(vec) <- rownames(data)
        vec
    }
)
