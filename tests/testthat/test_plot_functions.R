context("Plot Functions")



# Heatmaps =====================================================================
fxns <- c(
    "plotCorrelationHeatmap",
    "plotHeatmap",
    "plotQuantileHeatmap"
)

test_that("plotHeatmap : SummarizedExperiment", {
    invisible(lapply(fxns, function(f) {
        object <- rse_bcb
        f <- get(f)
        p <- f(object)

        # Expect pheatmap return
        expect_is(p, "pheatmap")
        expect_identical(names(p), pheatmapList)

        # Test that plots contain annotation data
        gtable <- p[["gtable"]]
        expect_true("annotation_legend" %in% gtable[["layout"]][["name"]])

        # Test color and title support
        expect_is(
            f(
                object = object,
                color = NULL,
                legendColor = NULL,
                title = NULL
            ),
            "pheatmap"
        )
        # Hexadecimal color functions (e.g. viridis)
        expect_is(
            f(
                object = object,
                color = viridis,
                legendColor = viridis
            ),
            "pheatmap"
        )
        # Hexadecimal color palettes (e.g. RColorBrewer)
        color <- colorRampPalette(brewer.pal(n = 11L, name = "PuOr"))(256L)
        expect_is(
            f(
                object = object,
                color = color
            ),
            "pheatmap"
        )
        # Disable interesting groups
        expect_is(
            f(
                object = object,
                interestingGroups = NULL
            ),
            "pheatmap"
        )
    }))
})

test_that("plotHeatmap : Invalid pheatmap passthrough", {
    expect_error(
        plotHeatmap(rse_bcb, show_colnames = FALSE),
        "Define formalArgs in camel case: show_colnames"
    )
})



# themes =======================================================================
p <- ggplot2::ggplot(
    data = ggplot2::mpg,
    mapping = ggplot2::aes(cty, hwy)
) +
    ggplot2::geom_point(color = "orange")

test_that("theme_midnight", {
    expect_is(theme_midnight(), "theme")
    x <- p + theme_midnight()
    expect_is(x, "ggplot")
    # Check background color
    expect_identical(
        x[["theme"]][["plot.background"]][["fill"]],
        "black"
    )
    # Grid mode
    x <- p + theme_midnight(grid = TRUE)
    expect_identical(
        x[["theme"]][["panel.grid.major"]][["colour"]],
        "gray10"
    )
})

test_that("theme_paperwhite", {
    expect_is(theme_paperwhite(), "theme")
    x <- p + theme_paperwhite()
    expect_is(x, "ggplot")
    # Check background color
    expect_identical(
        x[["theme"]][["plot.background"]][["fill"]],
        NULL
    )
    # Grid mode
    x <- p + theme_paperwhite(grid = TRUE)
    expect_identical(
        x[["theme"]][["panel.grid.major"]][["colour"]],
        "gray95"
    )
})
