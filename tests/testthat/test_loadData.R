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
    loaded <- loadData(mtcars, quiet = TRUE)
    rm(mtcars)
    expect_identical(
        loaded,
        c(mtcars = file.path(getwd(), "mtcars.rda"))
    )

    expect_message(
        suppressWarnings(loadData(mtcars)),
        paste("Loading mtcars.rda from", getwd())
    )

    # Abort on existing
    expect_error(
        loadData(mtcars, quiet = TRUE),
        "mtcars already exists in environment"
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        loadData(multi, quiet = TRUE),
        "is_a_string"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadData(mtcars, dir = NULL),
        "is_a_string"
    )
    expect_error(
        loadData(mtcars, dir = "XXX"),
        "is_existing_file"
    )
    expect_error(
        loadData(mtcars, envir = "XXX"),
        "is_environment"
    )
})

test_that("Renamed file", {
    expect_error(
        loadData(renamed),
        "are_identical"
    )
})

unlink(c("multi.rda", "mtcars.rda", "renamed.rda"))
