#' @name topCellsPerSample
#' @inherit AcidGenerics::topCellsPerSample
#' @note Updated 2020-01-30.
#'
#' @inheritParams AcidRoxygen::params
#' @param n `integer(1)`.
#'   Number of barcodes to return per sample.
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' x <- topCellsPerSample(object)
#' lapply(x, head)
NULL



#' @rdname topCellsPerSample
#' @name topCellsPerSample
#' @importFrom AcidGenerics topCellsPerSample
#' @usage topCellsPerSample(object, ...)
#' @export
NULL



## Updated 2020-01-30.
`topCellsPerSample,SingleCellExperiment` <-  # nolint
    function(object, n = 100L) {
        validObject(object)
        assert(isInt(n))
        cell2sample <- cell2sample(object)
        counts <- counts(object)
        if (is(counts, "Matrix")) {
            colSums <- Matrix::colSums
        }
        colSums <- colSums(counts)
        assert(identical(names(cell2sample), names(colSums)))
        data <- DataFrame(
            cellID = names(cell2sample),
            sampleID = cell2sample,
            n = colSums
        )
        split <- split(data, f = data[["sampleID"]])
        assert(is(split, "SplitDataFrameList"))
        lapply(
            X = split,
            FUN = function(x) {
                order <- order(x[["n"]], decreasing = TRUE)
                x <- x[order, , drop = FALSE]
                x <- head(x, n = n)
                x[["cellID"]]
            }
        )
    }



#' @rdname topCellsPerSample
#' @export
setMethod(
    f = "topCellsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = `topCellsPerSample,SingleCellExperiment`
)
