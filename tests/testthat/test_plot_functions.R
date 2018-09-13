context("Plot Functions")



# Heatmaps =====================================================================
funs <- list(
    plotCorrelationHeatmap,
    plotHeatmap,
    plotQuantileHeatmap
)
pheatmapList <- c("tree_row", "tree_col", "kmeans", "gtable")

with_parameters_test_that(
    "plotHeatmap : SummarizedExperiment", {
        object <- rse_small
        p <- f(object)

        # Expect pheatmap return.
        expect_is(p, "pheatmap")
        expect_identical(names(p), pheatmapList)

        # Test that plots contain annotation data.
        gtable <- p[["gtable"]]
        expect_true("annotation_legend" %in% gtable[["layout"]][["name"]])

        # Test color and title support.
        expect_is(
            f(
                object = object,
                color = NULL,
                legendColor = NULL,
                title = NULL
            ),
            "pheatmap"
        )

        # Hexadecimal color functions (e.g. viridis).
        expect_is(
            f(
                object = object,
                color = viridis::viridis,
                legendColor = viridis::viridis
            ),
            "pheatmap"
        )

        # Hexadecimal color palettes (e.g. RColorBrewer).
        color <- colorRampPalette(
            RColorBrewer::brewer.pal(n = 11L, name = "PuOr")
        )(256L)
        expect_is(
            f(
                object = object,
                color = color
            ),
            "pheatmap"
        )

        # Disable interesting groups.
        expect_is(
            f(
                object = object,
                interestingGroups = NULL
            ),
            "pheatmap"
        )
    },
    f = funs
)

test_that("plotHeatmap : Invalid pheatmap passthrough", {
    expect_error(
        plotHeatmap(rse_small, show_colnames = FALSE),
        "Define formalArgs in camel case: show_colnames"
    )
})
