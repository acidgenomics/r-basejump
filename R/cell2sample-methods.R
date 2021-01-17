#' @name cell2sample
#' @inherit AcidGenerics::cell2sample
#'
#' @note `sampleId` column must be defined in
#' [`colData()`][SummarizedExperiment::colData].
#' @note Updated 2021-01-17.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return
#' - `"factor"`: Named `factor` containing as the [levels][base::levels] and
#'   cell IDs as the [names][base::names].
#' - `DataFrame`: Data frame containing `sampleId` column and cell identifiers
#'   as the row names.
#' - `"tbl_df"`: Tibble containing `cellId` and `sampleId` columns.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#' sce <- SingleCellExperiment
#'
#' ## SingleCellExperiment ====
#' x <- cell2sample(sce)
#' table(x)
NULL



## Updated 2021-01-17.
`cell2sample,SingleCellExperiment` <-  # nolint
    function(
        object,
        return = c("factor", "DataFrame", "tbl_df")
    ) {
        validObject(object)
        return <- match.arg(return)
        colData <- colData(object)
        sampleCol <- matchSampleColumn(colData)
        assert(isSubset(sampleCol, colnames(colData)))
        cells <- colnames(object)
        samples <- colData[[sampleCol]]
        if (!is.factor(samples)) {
            samples <- as.factor(samples)
        }
        switch(
            EXPR = return,
            "DataFrame" = {
                out <- DataFrame(
                    "cellId" = cells,
                    "sampleId" = samples,
                    row.names = cells
                )
            },
            "factor" = {
                out <- samples
                names(out) <- cells
            },
            "tbl_df" = {
                out <- tibble(
                    "cellId" = cells,
                    "sampleId" = samples
                )
            }
        )
        out
    }



#' @rdname cell2sample
#' @export
setMethod(
    f = "cell2sample",
    signature = signature("SingleCellExperiment"),
    definition = `cell2sample,SingleCellExperiment`
)
