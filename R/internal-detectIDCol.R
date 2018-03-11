#' Detect ID Column
#'
#' Utility function for automatically setting rownames. Note that the transcript
#' ID column takes priority over the gene ID column.
#'
#' @keywords internal
#' @noRd
.detectIDCol <- function(object) {
    object <- as.data.frame(object)
    txCol <- match("txID", colnames(object)) %>%
        na.omit()
    geneCol <- match("geneID", colnames(object)) %>%
        na.omit()
    if (length(txCol)) {
        index <- txCol[[1L]]
    } else if (length(geneCol)) {
        index <- geneCol[[1L]]
    } else {
        abort("Failed to detect transcript or gene ID column")
    }
    colnames(object)[[index]]
}
