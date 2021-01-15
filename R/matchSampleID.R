#' Match sample identifier column
#'
#' @export
#' @note Updated 2021-01-15.
#'
#' @param object `SummarizedExperiment`.
#'
#' @details
#' Supports (in order of preference):
#' - `sampleId` (preferred).
#' - `sampleID` (legacy).
#' - `sampleid` (unused).
#' - `sample` (last resort).
#'
#' @return `character(1)` or error on match failure.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#' object <- SingleCellExperiment
#' id <- matchSampleID(object)
#' id
matchSampleID <- function(object) {
    assert(is(object, "SummarizedExperiment"))
    x <- colnames(colData(object))
    table <- c("sampleId", "sampleID", "sampleid", "sample")
    match <- match(x = x, table = table)
    if (all(is.na(match))) {
        stop(sprintf(
            paste0(
                "Failed to match sample identifier in '%s()'.\n",
                "Expecting (in order of preference): %s."
            ),
            "colData", toString(table)
        ))
    }
    id <- table[min(na.omit(match))]
    id
}
