context("loadRemoteData")

url <- "http://basejump.seq.cloud/mtcars.rda"

test_that("Valid URL", {
    loaded <- loadRemoteData(url = url, quiet = TRUE)
    expect_is(loaded, "matrix")
    expect_identical(
        loaded["url", "mtcars", drop = TRUE],
        url
    )
})

test_that("Already exists", {
    mtcars <- datasets::mtcars
    expect_error(
        loadRemoteData(url),
        "Already exists in environment: mtcars"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadRemoteData("http://basejump.seq.cloud/mmusculus.gtf"),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadData("http://basejump.seq.cloud/mtcars.rda", envir = "XXX"),
        "is_environment : envir"
    )
})
