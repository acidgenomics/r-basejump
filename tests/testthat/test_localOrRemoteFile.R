context("localOrRemoteFile")

test_that("Vectorized", {
    urls <- c(
        "http://basejump.seq.cloud/mtcars.csv",
        "http://basejump.seq.cloud/mtcars.rda")
    files <- localOrRemoteFile(urls)
    expect_is(files, "character")
    expect_identical(names(files), basename(urls))
})
