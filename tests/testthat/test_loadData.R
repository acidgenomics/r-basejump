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
        c(mtcars = path_join(c(path_real("."), "mtcars.rda")))
    )
    expect_message(
        suppressWarnings(loadData(mtcars)),
        paste("Loading mtcars.rda from", path_real("."))
    )
})

test_that("Multiple objects in single file", {
    expect_error(
        loadData(multi, quiet = TRUE),
        "is_a_string : loaded has length 2, not 1."
    )
})

test_that("Already exists", {
    mtcars <- datasets::mtcars
    expect_error(
        loadData(mtcars, quiet = TRUE),
        "Already exists in environment: mtcars"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadData(mtcars, dir = "XXX"),
        "is_dir : "
    )
    expect_error(
        loadData(mtcars, envir = "XXX"),
        "is_environment : envir"
    )
})

test_that("Renamed file", {
    expect_error(
        loadData(renamed),
        "are_identical : name and loaded are not identical."
    )
})

unlink(c("multi.rda", "mtcars.rda", "renamed.rda"))
