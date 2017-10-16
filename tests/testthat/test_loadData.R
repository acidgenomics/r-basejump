context("loadData")

utils::download.file(
    url = file.path(testDataURL, "mtcars.rda"),
    destfile = "mtcars.rda")

test_that("loadData", {
    x <- loadData(mtcars, dir = getwd())
    expect_equal(
        names(x),
        "mtcars"
    )
    expect_equal(
        basename(x),
        "mtcars.rda"
    )
    expect_error(
        loadData(foobar),
        "foobar missing"
    )
})

test_that("loadDataAsName", {
    x <- loadDataAsName(
        mappings = c(test = "mtcars"),
        dir = getwd()
    )
    expect_equal(
        names(x),
        "test"
    )
    expect_equal(
        basename(x),
        "mtcars.rda"
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
