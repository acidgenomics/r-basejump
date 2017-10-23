context("loadRemoteData")

test_that("loadRemoteData", {
    invisible <- loadRemoteData(
        "http://basejump.seq.cloud/mtcars.rda",
        quiet = TRUE)
    expect_equal(
        invisible,
        "mtcars"
    )
    expect_error(
        loadRemoteData("http://basejump.seq.cloud/mmusculus.gtf"),
        "Data file must contain '.rda' extension"
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "Remote URL containing '://' required"
    )
})
