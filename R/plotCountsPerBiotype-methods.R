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
        assert_is_a_string(countsAxisLabel)

        rowData <- rowData(object)

        # Determine whether to use transcripts or genes automatically.
        if ("transcriptBiotype" %in% colnames(rowData)) {
            biotypeCol <- "transcriptBiotype"
        } else {
            biotypeCol <- "geneBiotype"
        }
        assert_is_subset(biotypeCol, colnames(rowData))

        rowData <- rowData[, biotypeCol, drop = FALSE] %>%
            as("tbl_df")

        nCol <- sym("n")
        biotypeCol <- sym(biotypeCol)
        biotypes <- rowData %>%
            group_by(!!biotypeCol) %>%
            summarize(n = n()) %>%
            # Require at least 10 genes.
            filter(!!sym("n") >= 10L) %>%
            arrange(desc(!!nCol)) %>%
            top_n(n = n, wt = !!nCol) %>%
            pull(!!biotypeCol) %>%
            as.character()

        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID")

        counts <- assays(object)[[assay]]

        data <- counts %>%
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

        p <- ggplot(
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

        p
    }



# FIXME Figure out how to share formals with above function.
.plotCountsPerBroadClass.SE <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        trans = "log2",
        countsAxisLabel = "counts"
    ) {
        validObject(object)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        interestingGroups(object) <- interestingGroups
        assert_is_a_string(trans)
        assert_is_a_string(countsAxisLabel)

        rowData <- rowData(object)
        biotypeCol <- "broadClass"
        assert_is_subset(biotypeCol, colnames(rowData))
        rowData <- rowData[, biotypeCol, drop = FALSE] %>%
            as("tbl_df")

        nCol <- sym("n")
        biotypeCol <- sym(biotypeCol)
        biotypes <- rowData %>%
            group_by(!!biotypeCol) %>%
            summarize(n = n()) %>%
            # Require at least 10 genes.
            filter(!!sym("n") >= 10L) %>%
            arrange(desc(!!nCol)) %>%
            pull(!!biotypeCol) %>%
            as.character()

        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID")

        counts <- assays(object)[[assay]]

        data <- counts %>%
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

        p <- ggplot(
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
                title = "counts per broad class",
                x = NULL,
                y = countsAxisLabel,
                fill = paste(interestingGroups, collapse = ":\n")
            ) +
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank()
            )

        p
    }



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBiotype",
    signature = signature("SummarizedExperiment"),
    definition = .plotCountsPerBiotype.SE
)



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature("SummarizedExperiment"),
    definition = .plotCountsPerBroadClass.SE
)
