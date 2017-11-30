context("loadDataAsName")

utils::download.file(
    url = "http://basejump.seq.cloud/multi.rda",
    destfile = "multi.rda",
    quiet = TRUE)
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
        replace = FALSE
    )
    # Variable file paths (more flexible, but requires more typing)
    loaded2 <- suppressWarnings(
        loadDataAsName(
            newName1 = "mtcars.rda",
            newName2 = "starwars.rda",
            replace = TRUE)
    )
    expect_equal(
        loaded,
        c(newName1 = file.path(getwd(), "mtcars.rda"),
          newName2 = file.path(getwd(), "starwars.rda"))
    )
    expect_identical(loaded, loaded2)

    # Replace argument
    expect_warning(
        loadDataAsName(newName1 = "mtcars", replace = FALSE),
        "Skipping mtcars.rda because newName1 already exists"
    )
    expect_warning(
        loadDataAsName(newName1 = "mtcars", replace = TRUE),
        "Replacing newName1 with the contents of mtcars.rda"
    )

    # Error on first missing file
    expect_error(
        loadDataAsName(
            newName1 = "XXXXXX",
            newName2 = "YYYYYY",
            replace = TRUE
        ),
        "XXXXXX missing"
    )
})

test_that("Legacy named character method", {
    loaded <- loadDataAsName(c(test = "mtcars"))
    expect_equal(
        loaded,
        c(test = file.path(getwd(), "mtcars.rda"))
    )
    expect_error(
        loadDataAsName(c(test = "foobar")),
        "foobar missing"
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        loadDataAsName(newName = "multi"),
        "multi.rda contains multiple objects: x, y"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadDataAsName(newName = "mtcars", dir = NULL),
        "'dir' must be a string"
    )
    expect_error(
        loadDataAsName(newName = "mtcars", dir = "XXX"),
        "No directory exists at XXX"
    )
    expect_error(
        loadDataAsName(
            newName = "mtcars.rda",
            envir = "XXX"
        ),
        "'envir' must be an environment"
    )
})

unlink(c("multi.rda", "mtcars.rda", "starwars.rda"))
