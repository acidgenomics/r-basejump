#' @name matchSampleColumn
#' @inherit AcidGenerics::matchSampleColumn
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
`matchSampleColumn,DataFrame` <-  # nolint
    function(object) {
        x <- colnames(object)
        table <- c("sampleId", "sampleID", "sampleid", "sample")
        match <- match(x = x, table = table)
        if (all(is.na(match))) return(NULL)
        id <- table[min(na.omit(match))]
        id
    }



#' @rdname matchSampleColumn
#' @export
setMethod(
    f = "matchSampleColumn",
    signature = signature("DataFrame"),
    definition = `matchSampleColumn,DataFrame`
)



## Updated 2021-01-16.
`matchSampleColumn,SummarizedExperiment` <-  # nolint
    function(object) {
        matchSampleColumn(colData(object))
}



#' @rdname matchSampleColumn
#' @export
setMethod(
    f = "matchSampleColumn",
    signature = signature("SummarizedExperiment"),
    definition = `matchSampleColumn,SummarizedExperiment`
)
