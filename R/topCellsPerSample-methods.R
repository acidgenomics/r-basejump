#' @name topCellsPerSample
#' @inherit bioverbs::topCellsPerSample
#' @note Updated 2019-08-14.
#'
#' @inheritParams acidroxygen::params
#' @param n `integer(1)`.
#'   Number of barcodes to return per sample.
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' x <- topCellsPerSample(object)
#' lapply(x, head)
NULL



#' @rdname topCellsPerSample
#' @name topCellsPerSample
#' @importFrom bioverbs topCellsPerSample
#' @usage topCellsPerSample(object, ...)
#' @export
NULL



## Updated 2019-08-14.
`topCellsPerSample,SingleCellExperiment` <-  # nolint
    function(object, n = 100L) {
        validObject(object)
        assert(isInt(n))
        cell2sample <- cell2sample(object)
        counts <- counts(object)
        if (is(counts, "sparseMatrix")) {
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
