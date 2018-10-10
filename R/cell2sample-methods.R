#' Cell-to-Sample Mappings
#'
#' @note `sampleID` column must be defined in [colData()].
#'
#' @name cell2sample
#' @family SingleCellExperiment Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return
#' - `"factor"`: Named `factor` containing sample IDs as the levels and cell IDs
#'   as the names.
#' - `"tibble"`: `tbl_df` containing `cellID` and `sampleID` columns.
#'
#' @examples
#' data(sce_small)
#' x <- cell2sample(sce_small)
#' table(x)
NULL



.cell2sample.SCE <-  # nolint
    function(
        object,
        return = c("factor", "tibble")
    ) {
        validObject(object)
        assert_is_subset("sampleID", colnames(colData(object)))
        return <- match.arg(return)

        colData <- colData(object)

        if (return == "factor") {
            sample <- colData[["sampleID"]]
            assert_is_factor(sample)
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
    definition = .cell2sample.SCE
)
