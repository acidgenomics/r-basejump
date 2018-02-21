context("localOrRemoteFile")

test_that("Vectorized", {
    files <- localOrRemoteFile(c(
        "http://basejump.seq.cloud/mtcars.csv",
        "http://basejump.seq.cloud/mtcars.rda"
    ))
    expect_is(files, "list")
    expect_true(all(vapply(
        X = files,
        FUN = function(x) is(x, "fs_path"),
        FUN.VALUE = logical(1L)
    )))
})
