#' @name interestingGroups
#' @inherit AcidGenerics::interestingGroups
#' @note Updated 2021-01-16.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(RangedSummarizedExperiment, package = "AcidTest")
#' rse <- RangedSummarizedExperiment
#'
#' ## SummarizedExperiment ====
#' id <- matchSampleColumn(rse)
#' print(id)
NULL



## Updated 2021-01-16.
`matchSampleColumn,SummarizedExperiment` <-  # nolint
    function(object) {
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



#' @rdname matchSampleColumn
#' @export
setMethod(
    f = "matchSampleColumn",
    signature = signature("SummarizedExperiment"),
    definition = `matchSampleColumn,SummarizedExperiment`
)
