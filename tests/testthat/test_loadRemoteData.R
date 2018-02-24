context("loadRemoteData")

test_that("loadRemoteData", {
    loaded <- loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/"))
    expect_is(loaded, "matrix")
    expect_identical(
        loaded["url", "mtcars", drop = TRUE],
        paste(cacheURL, "mtcars.rda", sep = "/")
    )

    # Object already exists in environment =====================================
    mtcars <- datasets::mtcars
    expect_error(
        loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/")),
        "Already exists in environment: mtcars"
    )

    # Invalid arguments ========================================================
    expect_error(
        loadRemoteData(paste(cacheURL, "mmusculus.gtf", sep = "/")),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/"), envir = "XXX"),
        "is_environment : envir"
    )
})
