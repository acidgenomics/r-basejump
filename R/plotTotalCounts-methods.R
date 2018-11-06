#' @name plotTotalCounts
#' @inherit basejump.generics::plotTotalCounts
#' @inheritParams params
#' @examples
#' data(rse, sce, package = "basejump.data")
#' plotTotalCounts(rse)
#' plotTotalCounts(sce)
NULL



#' @importFrom basejump.generics plotTotalCounts
#' @aliases NULL
#' @export
basejump.generics::plotTotalCounts



# SummarizedExperiment =========================================================
plotTotalCounts.SummarizedExperiment <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        perMillion = FALSE,
        fill = getOption("basejump.discrete.fill", NULL),
        flip = getOption("basejump.flip", TRUE),
        title = "total counts"
    ) {
        validObject(object)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        interestingGroups(object) <- interestingGroups
        assert_is_a_bool(perMillion)
        assertIsFillScaleDiscreteOrNULL(fill)
        assert_is_a_bool(flip)
        assertIsAStringOrNULL(title)

        counts <- assays(object)[[assay]]
        data <- sampleData(object) %>%
            as_tibble() %>%
            mutate(totalCounts = colSums(!!counts))

        yLab <- "counts"
        if (isTRUE(perMillion)) {
            data <- mutate(data, totalCounts = !!sym("totalCounts") / 1e6L)
            yLab <- paste(yLab, "per million")
        }

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym("totalCounts"),
                fill = !!sym("interestingGroups")
            )
        ) +
            geom_bar(
                color = "black",
                stat = "identity"
            ) +
            labs(
                title = title,
                x = NULL,
                y = yLab,
                fill = paste(interestingGroups, collapse = ":\n")
            )

        if (is(fill, "ScaleDiscrete")) {
            p <- p + fill
        }

        if (isTRUE(flip)) {
            p <- p + coord_flip()
        }

        if (identical(interestingGroups, "sampleName")) {
            p <- p + guides(fill = FALSE)
        }

        p
    }



# SingleCellExperiment =========================================================
plotTotalCounts.SingleCellExperiment <-  # nolint
    function(object) {
        do.call(
            what = plotTotalCounts,
            args = matchArgsToDoCall(
                args = list(
                    object = aggregateCellsToSamples(object)
                )
            )
        )
    }
formals(plotTotalCounts.SingleCellExperiment) <-
    formals(plotTotalCounts.SummarizedExperiment)



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SummarizedExperiment"),
    definition = plotTotalCounts.SummarizedExperiment
)



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SingleCellExperiment"),
    definition = plotTotalCounts.SingleCellExperiment
)
