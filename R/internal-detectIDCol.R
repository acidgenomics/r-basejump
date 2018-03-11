#' Detect ID Column
#'
#' Utility function for automatically setting rownames. Note that the transcript
#' ID column takes priority over the gene ID column.
#'
#' @noRd
.detectIDCol <- function(object) {
    object <- as.data.frame(object)
    txCol <- match("txID", colnames(object))
    geneCol <- match("geneID", colnames(object))
    if (length(txCol)) {
        txCol[[1L]]
    } else if (length(geneCol)) {
        geneCol[[1L]]
    } else {
        abort("Failed to detect transcript or gene ID column")
    }
}
