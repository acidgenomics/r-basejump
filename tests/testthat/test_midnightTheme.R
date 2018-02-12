context("midnightTheme")

test_that("Simple plot", {
    p <- ggplot(mpg, aes(cty, hwy)) +
        ggplot2::geom_point(color = "orange") +
        midnightTheme()
    expect_is(p, "ggplot")
    # Check for the black plot background
    expect_identical(
        p[["theme"]][["plot.background"]][["fill"]],
        "black"
    )
})
