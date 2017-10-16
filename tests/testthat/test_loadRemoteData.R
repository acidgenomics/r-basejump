context("loadRemoteData")

test_that("loadRemoteData", {
    expect_silent(
        loadRemoteData(file.path(testDataURL, "mtcars.rda"))
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
