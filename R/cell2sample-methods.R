#' @name cell2sample
#' @inherit bioverbs::cell2sample
#'
#' @note `sampleID` column must be defined in
#' [`colData()`][SummarizedExperiment::colData].
#' @note Updated 2019-08-18.
#'
#' @inheritParams acidroxygen::params
#' @param ... Additional arguments.
#'
#' @return
#' - `"factor"`: Named `factor` containing as the [levels][base::levels] and
#'   cell IDs as the [names][base::names].
#' - `"tbl_df"`: Tibble containing `cellID` and `sampleID` columns.
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



## Updated 2019-08-18.
`cell2sample,SingleCellExperiment` <-  # nolint
    function(object, return = c("factor", "tbl_df")) {
        validObject(object)
        assert(isSubset("sampleID", colnames(colData(object))))
        return <- match.arg(return)
        data <- colData(object)
        data <- data[, "sampleID", drop = FALSE]
        data <- decode(data)
        if (identical(return, "factor")) {
            out <- data[["sampleID"]]
            assert(is.factor(out))
            names(out) <- colnames(object)
        } else if (identical(return, "tbl_df")) {
            out <- as_tibble(data, rownames = "cellID")
        }
        out
    }



#' @rdname cell2sample
#' @export
setMethod(
    f = "cell2sample",
    signature = signature("SingleCellExperiment"),
    definition = `cell2sample,SingleCellExperiment`
)
