#' @name topCellsPerSample
#'
#' @param n `scalar integer`. Number of barcodes to return per sample.
#'
#' @examples
#' data(sce)
#' x <- topCellsPerSample(sce)
#' lapply(x, head)
NULL



topCellsPerSample.SingleCellExperiment <-  # nolint
    function(object, n = 100L) {
        validObject(object)
        assertIsAnImplicitInteger(n)

        cell2sample <- cell2sample(object)
        colSums <- Matrix::colSums(counts(object))
        assert_are_identical(names(cell2sample), names(colSums))

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
