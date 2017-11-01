context("loadData")

test_that("loadData", {
    invisible <- loadData(mtcars, dir = getwd(), quiet = TRUE)
    expect_equal(
        basename(invisible),
        "mtcars.rda"
    )
    expect_equal(
        names(invisible),
        "mtcars"
    )
    expect_error(
        loadData(foobar, quiet = TRUE),
        "foobar missing"
    )
})
