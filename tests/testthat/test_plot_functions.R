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



# midnightTheme ================================================================
test_that("midnightTheme", {
    p <- ggplot2::ggplot(
        data = mpg,
        mapping = ggplot2::aes(cty, hwy)
    ) +
        ggplot2::geom_point(color = "orange") +
        midnightTheme()
    expect_is(p, "ggplot")
    # Check for the black plot background
    expect_identical(
        p[["theme"]][["plot.background"]][["fill"]],
        "black"
    )
})
