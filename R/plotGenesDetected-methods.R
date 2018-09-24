#' Plot Genes Detected
#'
#' @name plotGenesDetected
#' @family Quality Control Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @examples
#' plotGenesDetected(rse_small)
NULL



.plotGenesDetected.SE <-  # nolint
    function(
        object,
        assay = 1L,
        interestingGroups = NULL,
        limit = 0L,
        minCounts = 1L,
        fill = getOption("basejump.discrete.fill", NULL),
        flip = getOption("basejump.flip", TRUE),
        title = "genes detected"
    ) {
        validObject(object)
        assert_is_scalar(assay)
        interestingGroups <- matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups
        )
        interestingGroups(object) <- interestingGroups
        assertIsAnImplicitInteger(limit)
        assert_all_are_non_negative(limit)
        assertIsAnImplicitInteger(minCounts)
        assert_all_are_in_range(minCounts, lower = 1L, upper = Inf)
        assert_all_are_non_negative(minCounts)
        assertIsFillScaleDiscreteOrNULL(fill)
        assert_is_a_bool(flip)
        assertIsAStringOrNULL(title)

        counts <- assays(object)[[assay]]
        geneCount <- colSums(counts >= minCounts)
        data <- metrics(object) %>%
            as_tibble() %>%
            mutate(geneCount = !!geneCount)

        p <- ggplot(
            data = data,
            mapping = aes(
                x = !!sym("sampleName"),
                y = !!sym("geneCount"),
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
                y = "gene count",
                fill = paste(interestingGroups, collapse = ":\n")
            )

        if (is_positive(limit)) {
            p <- p + basejump_geom_abline(yintercept = limit)
        }

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



#' @rdname plotGenesDetected
#' @export
setMethod(
    f = "plotGenesDetected",
    signature = signature("SummarizedExperiment"),
    definition = .plotGenesDetected.SE
)
