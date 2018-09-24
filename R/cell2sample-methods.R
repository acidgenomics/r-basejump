#' Cell to Sample Mappings
#'
#' @note `sampleID` column must be defined in [colData()].
#'
#' @name cell2sample
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @return Named `factor` containing sample IDs as the levels and cell IDs as
#'   the names.
#'
#' @examples
#' x <- cell2sample(sce_small)
#' table(x)
NULL



.cell2sample.SCE <-  # nolint
    function(object) {
        validObject(object)
        assert_is_subset("sampleID", colnames(colData(object)))
        sample <- colData(object)[["sampleID"]]
        assert_is_factor(sample)
        cell <- colnames(object)
        names(sample) <- cell
        sample
    }



#' @rdname cell2sample
#' @export
setMethod(
    f = "cell2sample",
    signature = signature("SingleCellExperiment"),
    definition = .cell2sample.SCE
)
