#' Plot Counts per Biotype
#'
#' @name plotCountsPerBiotype
#' @family Plot Functions
#' @author Michael Steinbaugh, Rory Kirchner
#' @export
#'
#' @inheritParams general
#'
#' @return `ggplot`.
#'
#' @seealso [makeGRanges].
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

        # Coerce the row data to a tibble containing our column of interest.
        rowData <- rowData[, biotypeCol, drop = FALSE] %>%
            as("tbl_df")

        # Get the top biotypes from the row data.
        biotypes <- rowData %>%
            group_by(!!sym(biotypeCol)) %>%
            summarize(n = n()) %>%
            # Require at least 10 genes.
            filter(!!sym("n") >= 10L) %>%
            arrange(desc(!!sym("n"))) %>%
            top_n(n = n, wt = !!sym("n")) %>%
            pull(!!sym(biotypeCol)) %>%
            as.character()

        # Coerce the sample data to a tibble.
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID")))

        # Gather the counts into a long tibble.
        data <- assays(object)[[assay]] %>%
            # Ensure sparse matrix is coerced to dense.
            as.matrix() %>%
            as_tibble(rownames = "rowname") %>%
            gather(
                key = "colname",
                value = "counts",
                -UQ(sym("rowname"))
            )

        # SingleCellExperiment requires cell2sample mapping.
        if (is(object, "SingleCellExperiment")) {
            c2s <- cell2sample(object, return = "tibble") %>%
                rename(!!sym("colname") := !!sym("cellID"))
            data <- left_join(data, c2s, by = "colname")
        } else {
            data <- rename(data, !!sym("sampleID") := !!sym("colname"))
        }

        # Prepare the minimal tibble required for plotting.
        data <- data %>%
            # Drop zero counts, which is useful when log scaling the axis.
            filter(!!sym("counts") > 0L) %>%
            left_join(rowData, by = "rowname") %>%
            filter(!!sym(biotypeCol) %in% !!biotypes) %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID"))) %>%
            left_join(sampleData, by = "sampleID") %>%
            select(!!!syms(c("counts", "interestingGroups", biotypeCol)))

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
            facet_wrap(facets = sym(biotypeCol), scales = "free_y") +
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

        biotypes <- rowData %>%
            group_by(!!sym(biotypeCol)) %>%
            summarize(n = n()) %>%
            # Require at least 10 genes.
            filter(!!sym("n") >= 10L) %>%
            arrange(desc(!!sym("n"))) %>%
            pull(!!sym(biotypeCol)) %>%
            as.character()

        # Coerce the sample data to a tibble.
        sampleData <- sampleData(object) %>%
            as_tibble(rownames = "sampleID") %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID")))

        data <- assays(object)[[assay]] %>%
            # Ensure sparse matrix is coerced to dense.
            as.matrix() %>%
            as_tibble(rownames = "rowname") %>%
            gather(
                key = "colname",
                value = "counts",
                -UQ(sym("rowname"))
            )

        # SingleCellExperiment requires cell2sample mapping.
        if (is(object, "SingleCellExperiment")) {
            c2s <- cell2sample(object, return = "tibble") %>%
                rename(!!sym("colname") := !!sym("cellID"))
            data <- left_join(data, c2s, by = "colname")
        } else {
            data <- rename(data, !!sym("sampleID") := !!sym("colname"))
        }

        data <- data %>%
            filter(!!sym("counts") > 0L) %>%
            left_join(rowData, by = "rowname") %>%
            filter(!!sym(biotypeCol) %in% !!biotypes) %>%
            mutate(!!sym("sampleID") := as.factor(!!sym("sampleID"))) %>%
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
            facet_wrap(facets = sym(biotypeCol), scales = "free_y") +
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
    f = "plotCountsPerBiotype",
    signature = signature("SingleCellExperiment"),
    definition = getMethod(
        f = "plotCountsPerBiotype",
        signature = signature("SummarizedExperiment")
    )
)



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature("SummarizedExperiment"),
    definition = .plotCountsPerBroadClass.SE
)



#' @rdname plotCountsPerBiotype
#' @export
setMethod(
    f = "plotCountsPerBroadClass",
    signature = signature("SingleCellExperiment"),
    definition = getMethod(
        f = "plotCountsPerBroadClass",
        signature = signature("SummarizedExperiment")
    )
)
