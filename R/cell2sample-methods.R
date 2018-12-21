#' @name cell2sample
#' @inherit bioverbs::cell2sample
#' @inheritParams params
#'
#' @note `sampleID` column must be defined in
#' [colData][SummarizedExperiment::colData].
#'
#' @return
#' - `"factor"`: Named `factor` containing as the [levels][base::levels] and
#'   cell IDs as the [names][base::names].
#' - `"tibble"`: `tbl_df` containing `cellID` and `sampleID` columns.
#'
#' @examples
#' data(sce)
#' x <- cell2sample(sce)
#' table(x)
NULL



#' @importFrom bioverbs cell2sample
#' @aliases NULL
#' @export
bioverbs::cell2sample



cell2sample.SingleCellExperiment <-  # nolint
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
            colData %>%
                as_tibble(rownames = "cellID") %>%
                select(!!!syms(c("cellID", "sampleID")))
        }
    }



#' @rdname cell2sample
#' @export
setMethod(
    f = "cell2sample",
    signature = signature("SingleCellExperiment"),
    definition = cell2sample.SingleCellExperiment
)
