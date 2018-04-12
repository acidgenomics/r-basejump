context("Plot Functions")

# dynamicPlotlist ==============================================================
test_that("dynamicPlotlist", {
    # grid
    x <- dynamicPlotlist(plotlist, return = "grid")
    expect_s3_class(x, "ggplot")

    # list
    x <- dynamicPlotlist(plotlist, return = "list")
    expect_is(x, "list")

    # markdown
    output <- capture.output(
        dynamicPlotlist(plotlist, return = "markdown")
    )
    expect_identical(
        output,
        c(
            "",
            "",
            "## continuous",
            "",
            "",
            "",
            "## discrete",
            ""
        )
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
})
