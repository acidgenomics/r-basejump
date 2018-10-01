# FIXME Add plotting support for broadClass.



#' Plot Counts per Biotype
#'
#' @name plotCountsPerBiotype
#' @family Plot Functions
#' @author Rory Kirchner, Michael Steinbaugh
#' @export
#'
#' @examples
#' plotCountsPerBiotype(rse_small)
NULL



.plotCountsPerBiotype.SE <-  # nolint
    function(
        object,
        assay = 1L,
        n = 12L
    ) {
        validObject(object)
        assert_is_scalar(assay)
        assertIsAnImplicitInteger(n)

        assay <- assays(object)[[assay]]

        rowData <- as(rowData(object), "tbl_df")
        assert_is_subset("geneBiotype", colnames(rowData))

        biotypeCol <- sym("geneBiotype")
        nCol <- sym("n")
        biotypes <- rowData %>%
            group_by(!!biotypeCol) %>%
            summarise(n = n()) %>%
            arrange(desc(!!nCol)) %>%
            top_n(n = n, wt = !!nCol) %>%
            pull(!!biotypeCol) %>%
            droplevels()

        data <- assay %>%
            as_tibble(rownames = "geneID") %>%
            gather(
                key = "sampleID",
                value = "counts",
                -UQ(sym("geneID"))
            ) %>%
            filter(!!sym("counts") > 0L) %>%
            left_join(rowData, by = "geneID") %>%
            filter(!!biotypeCol %in% !!biotypes)

        ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleID"),
                y = !!sym("counts"),
                fill = !!sym("sampleID")
            )
        ) +
            # FIXME Improve the violin aes.
            geom_violin(
                color = "black",
                scale = "area"
            ) +
            scale_y_log10() +
            # FIXME Switch to tidyeval.
            facet_wrap(~geneBiotype, scales = "free_y") +
            # FIXME Fix the labels.
            labs(
                title = "counts per biotype",
                x = NULL,
                y = "counts"
            ) +
            guides(fill = FALSE)
    }



setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SummarizedExperiment"),
    definition = .plotCountsPerBiotype.SE
)
