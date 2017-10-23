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

test_that("loadDataAsName", {
    invisible <- loadDataAsName(
        mappings = c(test = "mtcars"),
        dir = getwd())
    expect_equal(
        basename(invisible),
        "mtcars.rda"
    )
    expect_equal(
        names(invisible),
        "test"
    )
    expect_error(
        loadDataAsName(
            mappings = c(test = "foobar"),
            dir = getwd()
        ),
        "foobar missing"
    )
})

unlink("mtcars.rda")
