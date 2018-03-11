#' Detect ID Column
#'
#' Utility function for automatically setting rownames. Note that the transcript
#' ID column takes priority over the gene ID column.
#'
#' @keywords internal
#' @noRd
.detectIDCol <- function(object) {
    return(object)
    object <- as.data.frame(object)
    assert_are_intersecting_sets(
        x = c("txID", "geneID"),
        y = colnames(object)
    )
    txCol <- match("txID", colnames(object)) %>%
        na.omit()
    geneCol <- match("geneID", colnames(object)) %>%
        na.omit()
    if (length(txCol)) {
        index <- txCol[[1L]]
    } else {
        index <- geneCol[[1L]]
    }
    colnames(object)[[index]]
}
