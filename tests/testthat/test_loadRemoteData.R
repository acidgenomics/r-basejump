context("loadRemoteData")

test_that("Valid URL", {
    loaded <- loadRemoteData(url = paste(url, "mtcars.rda", sep = "/"))
    expect_is(loaded, "matrix")
    expect_identical(
        loaded["url", "mtcars", drop = TRUE],
        paste(url, "mtcars.rda", sep = "/")
    )
})

test_that("Already exists", {
    mtcars <- datasets::mtcars
    expect_error(
        loadRemoteData(paste(url, "mtcars.rda", sep = "/")),
        "Already exists in environment: mtcars"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadRemoteData(paste(url, "mmusculus.gtf", sep = "/")),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadRemoteData(paste(url, "mtcars.rda", sep = "/"), envir = "XXX"),
        "is_environment : envir"
    )
})
