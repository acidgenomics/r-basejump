#' @name topCellsPerSample
#' @inherit bioverbs::topCellsPerSample
#' @note Updated 2019-07-28.
#'
#' @inheritParams acidroxygen::params
#' @param n `integer(1)`.
#'   Number of barcodes to return per sample.
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "acidtest")
#' sce <- SingleCellExperiment
#'
#' ## SingleCellExperiment ====
#' x <- topCellsPerSample(sce)
#' lapply(x, head)
NULL



#' @rdname topCellsPerSample
#' @name topCellsPerSample
#' @importFrom bioverbs topCellsPerSample
#' @usage topCellsPerSample(object, ...)
#' @export
NULL



## Updated 2019-07-22.
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
        tibble(
            cellID = names(cell2sample),
            sampleID = cell2sample,
            n = colSums
        ) %>%
            group_by(!!sym("sampleID")) %>%
            arrange(desc(!!sym("n")), .by_group = TRUE) %>%
            slice(seq_len(!!n)) %>%
            split(.[["sampleID"]]) %>%
            map("cellID")
    }



#' @rdname topCellsPerSample
#' @export
setMethod(
    f = "topCellsPerSample",
    signature = signature("SingleCellExperiment"),
    definition = `topCellsPerSample,SingleCellExperiment`
)
