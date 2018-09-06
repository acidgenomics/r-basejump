context("ggplot2 Functions")



# basejump_geom_abline =========================================================
test_that("basejump_geom_abline", {
    object <- basejump_geom_abline(xintercept = 1L)
    expect_is(object, "Layer")

    object <- basejump_geom_abline(yintercept = 1L)
    expect_is(object, "Layer")

    # Require single xintercept or yintercept
    error <- "Either `xintercept` or `yintercept` is required"
    expect_error(
        object = basejump_geom_abline(),
        regexp = error
    )
    expect_error(
        object = basejump_geom_abline(xintercept = 1L, yintercept = 1L),
        regexp = error
    )
})



# basejump_geom_label ==========================================================
test_that("basejump_geom_label", {
    object <- basejump_geom_label()
    expect_is(object, "Layer")
})



# basejump_geom_label_average ==================================================
test_that("basejump_geom_label_average", {
    # Normal mode.
    data <- data.frame(
        sampleName = c("sample1", "sample2"),
        counts = seq_len(8L)
    )
    object <- basejump_geom_label_average(data, col = "counts")
    expect_is(object, "Layer")

    # Aggregate mode, for facet wrapping.
    data <- data.frame(
        sampleName = c("sample1", "sample2"),
        aggregate = "sample",
        counts = seq_len(8L)
    )
    object <- basejump_geom_label_average(data, col = "counts")
    expect_is(object, "Layer")
})



# basejump_geom_label_repel ====================================================
test_that("basejump_geom_label_repel", {
    object <- basejump_geom_label_repel()
    expect_is(object, "Layer")

    # Single color mode.
    object <- basejump_geom_label_repel(color = "orange")
    expect_identical(
        object = object[["aes_params"]][["colour"]],
        expected = "orange"
    )
})



# themes =======================================================================
with_parameters_test_that(
    "themes", {
        p <- ggplot2::ggplot(
            data = ggplot2::mpg,
            mapping = ggplot2::aes(cty, hwy)
        ) +
            ggplot2::geom_point(color = "orange")
        expect_is(fun(), "theme")
        object <- p + fun()
        expect_is(object, "ggplot")
        # Check background color.
        expect_identical(
            object = object[["theme"]][["plot.background"]][["fill"]],
            expected = plotBackground
        )
        # Grid mode.
        object <- p + fun(grid = TRUE)
        expect_identical(
            object[["theme"]][["panel.grid.major"]][["colour"]],
            gridColor
        )
        # Minimal mode.
    },
    fun = list(
        paperwhite = theme_paperwhite,
        midnight = theme_midnight
    ),
    plotBackground = list(
        paperwhite = NULL,
        midnight = "black"
    ),
    gridColor = list(
        paperwhite = "gray95",
        midnight = "gray10"
    )
)
