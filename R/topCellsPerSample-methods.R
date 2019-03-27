#' @name topCellsPerSample
#' @inherit bioverbs::topCellsPerSample
#' @inheritParams params
#'
#' @param n `integer(1)`.
#'   Number of barcodes to return per sample.
#'
#' @examples
#' data(sce, package = "acidData")
#' x <- topCellsPerSample(sce)
#' lapply(x, head)
NULL



#' @importFrom bioverbs topCellsPerSample
#' @aliases NULL
#' @export
bioverbs::topCellsPerSample



topCellsPerSample.SingleCellExperiment <-  # nolint
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
    definition = topCellsPerSample.SingleCellExperiment
)
