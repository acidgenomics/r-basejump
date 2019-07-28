#' @name cell2sample
#' @inherit bioverbs::cell2sample
#'
#' @note `sampleID` column must be defined in
#' [`colData()`][SummarizedExperiment::colData].
#' @note Updated 2019-07-28.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return
#' - `"factor"`: Named `factor` containing as the [levels][base::levels] and
#'   cell IDs as the [names][base::names].
#' - `"tibble"`: `tbl_df` containing `cellID` and `sampleID` columns.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#' sce <- SingleCellExperiment
#'
#' ## SingleCellExperiment ====
#' x <- cell2sample(sce)
#' table(x)
NULL



#' @rdname cell2sample
#' @name cell2sample
#' @importFrom bioverbs cell2sample
#' @usage cell2sample(object, ...)
#' @export
NULL



## Updated 2019-07-22.
`cell2sample,SingleCellExperiment` <-  # nolint
    function(object, return = c("factor", "tibble")) {
        validObject(object)
        assert(isSubset("sampleID", colnames(colData(object))))
        return <- match.arg(return)
        colData <- colData(object)
        if (return == "factor") {
            sample <- colData[["sampleID"]]
            assert(is.factor(sample))
            cell <- colnames(object)
            names(sample) <- cell
            sample
        } else if (return == "tibble") {
            data <- as_tibble(colData, rownames = "cellID")
            data <- data[, c("cellID", "sampleID")]
            data
        }
    }



#' @rdname cell2sample
#' @export
setMethod(
    f = "cell2sample",
    signature = signature("SingleCellExperiment"),
    definition = `cell2sample,SingleCellExperiment`
)
