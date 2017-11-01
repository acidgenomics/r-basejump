context("loadData")

utils::download.file(
    url = "http://basejump.seq.cloud/mtcars.rda",
    destfile = "mtcars.rda",
    quiet = TRUE)

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

unlink("mtcars.rda")
