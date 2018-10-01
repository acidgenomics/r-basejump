# FIXME Add plotting support for broadClass.
# FIXME Detect gene or transcript-level automatically.
# FIXME Add SingleCellExperiment method support.



#' Plot Counts per Biotype
#'
#' @name plotCountsPerBiotype
#' @family Plot Functions
#' @author Rory Kirchner, Michael Steinbaugh
#' @export
#'
#' @seealso [broadClass()].
#'
#' @inheritParams general
#'
#' @examples
#' plotCountsPerBiotype(rse_small)
#' plotCountsPerBroadClass(rse_small)
NULL



.plotCountsPerBiotype.SE <-  # nolint
    function(
        object,
        assay = 1L,
        n = 9L,
        interestingGroups = NULL,
        trans = "log2",
        points = FALSE,
        countsAxisLabel = "counts"
    ) {
        validObject(object)
        assert_is_scalar(assay)
        assertIsAnImplicitInteger(n)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        interestingGroups(object) <- interestingGroups
        assert_is_a_string(trans)
        assert_is_a_bool(points)
        assert_is_a_string(countsAxisLabel)

        rowData <- rowData(object)
        # FIXME Detect gene/transcript automatically.
        biotypeCol <- "geneBiotype"
        assert_is_subset(biotypeCol, colnames(rowData))
        rowData <- rowData[, biotypeCol, drop = FALSE] %>%
            as("tbl_df")

        nCol <- sym("n")
        biotypeCol <- sym(biotypeCol)
        biotypes <- rowData %>%
            group_by(!!biotypeCol) %>%
            summarize(n = n()) %>%
            arrange(desc(!!nCol)) %>%
            top_n(n = n, wt = !!nCol) %>%
            pull(!!biotypeCol) %>%
            as.character()

        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID")

        assay <- assays(object)[[assay]]

        data <- assay %>%
            as.matrix() %>%
            as_tibble(rownames = "rowname") %>%
            gather(
                key = "sampleID",
                value = "counts",
                -UQ(sym("rowname"))
            ) %>%
            filter(!!sym("counts") > 0L) %>%
            left_join(rowData, by = "rowname") %>%
            filter(!!biotypeCol %in% !!biotypes) %>%
            left_join(sampleData, by = "sampleID")

        # FIXME Need to fix the interestingGroups legend label.
        ggplot(
            data = data,
            mapping = aes(
                x = !!sym("interestingGroups"),
                y = !!sym("counts")
            )
        ) +
            geom_violin(
                mapping = aes(fill = !!sym("interestingGroups")),
                color = "black",
                scale = "area",
                trim = TRUE
            ) +
            scale_y_continuous(trans = trans) +
            facet_wrap(facets = biotypeCol, scales = "free_y") +
            labs(
                title = "counts per biotype",
                x = NULL,
                y = countsAxisLabel,
                fill = paste(interestingGroups, collapse = ":\n")
            ) +
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank()
            )
    }



.plotCountsPerBroadClass.SE <-  # nolint
    function(object) {
        broad_tpm <- tpm(bcb) %>%
            as.data.frame() %>%
            rownames_to_column("geneID") %>%
            gather(key = sampleID, value = tpm, -geneID) %>%
            left_join(as.data.frame(rowData(bcb)), by = "geneID") %>%
            filter(!is.na(broadClass)) %>%
            filter(tpm > 0)

        ggplot(
            data = broad_tpm,
            mapping = aes(
                x = sampleID,
                y = tpm,
                fill = sampleID
            )
        ) +
            geom_violin(
                color = "black",
                scale = "area"
            ) +
            facet_wrap(~broadClass, scales = "free_y") +
            scale_y_log10() +
            labs(
                title = "tpm per broad biotype class",
                x = NULL,
                y = "transcripts per million (tpm)"
            ) +
            guides(fill = FALSE) +
            theme(axis.text.x = element_text(angle = 90L, hjust = 1L, vjust = 0.5))
    }



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SummarizedExperiment"),
    definition = .plotCountsPerBiotype.SE
)
