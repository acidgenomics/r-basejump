#' Top Cells
#'
#' Obtain the top cellular barcodes, based on counts.
#'
#' @name topCells
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#' @param n `scalar integer`. Number of barcodes to return per sample.
#'
#' @return `list`. Top barcodes as `character`, split by `sampleID`.
#'
#' @examples
#' x <- topCells(sce_small)
#' lapply(x, head)
NULL



.topCells.SCE <-  # nolint
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



#' @rdname topCells
#' @export
setMethod(
    f = "topCells",
    signature = signature("SingleCellExperiment"),
    definition = .topCells.SCE
)
