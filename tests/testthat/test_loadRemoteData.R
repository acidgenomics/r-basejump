context("loadRemoteData")

test_that("loadRemoteData", {
    invisible <- loadRemoteData(
        file.path(testDataURL, "mtcars.rda"),
        quiet = TRUE)
    expect_equal(
        invisible,
        "mtcars"
    )
    expect_error(
        loadRemoteData(file.path(testDataURL, "mmusculus.gtf")),
        "Data file must contain '.rda' extension"
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "Remote URL containing '://' required"
    )
})
