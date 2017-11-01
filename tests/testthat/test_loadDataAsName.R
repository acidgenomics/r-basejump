context("loadDataAsName")

utils::download.file(
    url = "http://basejump.seq.cloud/mtcars.rda",
    destfile = "mtcars.rda",
    quiet = TRUE)
utils::download.file(
    url = "http://basejump.seq.cloud/starwars.rda",
    destfile = "starwars.rda",
    quiet = TRUE)

test_that("Dot object key-value pair method", {
    # Short-hand method, using `dir` argument (preferred)
    loaded <- loadDataAsName(
        newName1 = "mtcars",
        newName2 = "starwars",
        dir = getwd()
    )
    # Variable file paths (more flexible, but requires more typing)
    loaded2 <- loadDataAsName(
        newName1 = "mtcars.rda",
        newName2 = "starwars.rda",
        dir = "XXX"  # ignored
    )
    expect_equal(
        loaded,
        c(newName1 = file.path(getwd(), "mtcars.rda"),
          newName2 = file.path(getwd(), "starwars.rda"))
    )
    expect_identical(loaded, loaded2)
    # Error on first missing file
    expect_error(
        loadDataAsName(
            newName1 = "XXXXXX",
            newName2 = "YYYYYY",
            dir = getwd()
        ),
        "XXXXXX missing"
    )
})

test_that("Legacy named character method", {
    loaded <- loadDataAsName(
        mappings = c(test = "mtcars"),
        dir = getwd())
    expect_equal(
        loaded,
        c(test = file.path(getwd(), "mtcars.rda"))
    )
    expect_error(
        loadDataAsName(
            mappings = c(test = "foobar"),
            dir = getwd()
        ),
        "foobar missing"
    )
})

unlink(c("mtcars.rda", "starwars.rda"))
