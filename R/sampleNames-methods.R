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



# Methods ======================================================================
#' @rdname sampleData
#' @export
setMethod(
    "sampleNames",
    signature("SummarizedExperiment"),
    function(object) {
        data <- sampleData(object)
        data <- data[sort(rownames(data)), , drop = FALSE]
        if ("sampleName" %in% colnames(data)) {
            vec <- data[, "sampleName", drop = TRUE]
        } else {
            vec <- rownames(data)
        }
        vec <- as.character(vec)
        names(vec) <- rownames(data)
        vec
    }
)
