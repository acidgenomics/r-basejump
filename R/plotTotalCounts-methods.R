#' Plot Total Counts
#'
#' @name plotTotalCounts
#' @family Quality Control Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#' @param perMillion `boolean`. Display as counts per million.
#'
#' @examples
#' plotTotalCounts(bcb_small)
NULL



.plotTotalCounts.SE <-  # nolint
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
            as("tbl_df") %>%
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



#' @rdname plotTotalCounts
#' @export
setMethod(
    f = "plotTotalCounts",
    signature = signature("SummarizedExperiment"),
    definition = .plotTotalCounts.SE
)
