context("loadRemoteData")

test_that("loadRemoteData", {
    loaded <- loadRemoteData(
        "http://basejump.seq.cloud/mtcars.rda",
        quiet = TRUE)
    expect_equal(loaded, "mtcars")
    expect_error(
        loadRemoteData("http://basejump.seq.cloud/mmusculus.gtf"),
        "Data file must contain '.rda' extension"
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "Remote URL containing '://' required"
    )
})

test_that("Invalid arguments", {
    expect_error(
        loadRemoteData(c("XXX", "YYY")),
        "'object' must be a string"
    )
    expect_error(
        loadData("http://basejump.seq.cloud/mtcars.rda", envir = "XXX"),
        "'envir' must be an environment"
    )
})
