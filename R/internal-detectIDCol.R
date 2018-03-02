#' Detect ID Column
#'
#' Utility function for automatically setting rownames. Note that the transcript
#' ID column takes priority over the gene ID column.
#'
#' @noRd
.detectIDCol <- function(object) {
    assert_has_colnames(object)
    txCol <- grep("enstxp|txID", colnames(data), value = TRUE)
    geneCol <- grep("ensgene|geneID", colnames(data), value = TRUE)
    if (length(txCol)) {
        txCol[[1L]]
    } else if (length(geneCol)) {
        geneCol[[1L]]
    } else {
        abort("Failed to detect ID column")
    }
}
