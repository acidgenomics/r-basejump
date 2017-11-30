context("loadData")

utils::download.file(
    url = "http://basejump.seq.cloud/multi.rda",
    destfile = "multi.rda",
    quiet = TRUE)
utils::download.file(
    url = "http://basejump.seq.cloud/mtcars.rda",
    destfile = "mtcars.rda",
    quiet = TRUE)
utils::download.file(
    url = "http://basejump.seq.cloud/renamed.rda",
    destfile = "renamed.rda",
    quiet = TRUE)

test_that("loadData", {
    loaded <- loadData(mtcars, replace = TRUE, quiet = TRUE)
    expect_equal(
        loaded,
        c(mtcars = file.path(getwd(), "mtcars.rda"))
    )
    expect_message(
        suppressWarnings(
            loadData(mtcars, replace = TRUE)
        ),
        paste("Loading mtcars from", getwd())
    )

    # Replace argument
    expect_warning(
        loadData(mtcars, replace = TRUE),
        paste("Replacing mtcars with the contents of mtcars.rda")
    )
    expect_warning(
        loadData(mtcars, replace = FALSE, quiet = TRUE),
        "Skipping mtcars.rda because mtcars already exists"
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        loadData(multi),
        "multi.rda contains multiple objects: x, y"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadData(mtcars, dir = NULL),
        "'dir' must be a string"
    )
    expect_error(
        loadData(mtcars, dir = "XXX"),
        "No directory exists at XXX"
    )
    expect_error(
        loadData(mtcars, envir = "XXX"),
        "'envir' must be an environment"
    )
})

test_that("Missing file", {
    expect_error(
        loadData(foobar, quiet = TRUE),
        "foobar missing"
    )
})

test_that("Renamed file", {
    expect_error(
        loadData(renamed),
        paste("Name mismatch detected for 'renamed.rda'.",
              "Internal object is named 'x'.")
    )
})

unlink(c("multi.rda", "mtcars.rda", "renamed.rda"))
