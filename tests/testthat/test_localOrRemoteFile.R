context("localOrRemoteFile")

test_that("Vectorized", {
    urls <- paste(url, c("mtcars.csv", "mtcars.rda"), sep = "/")
    files <- localOrRemoteFile(urls)
    expect_is(files, "fs_path")
    expect_identical(names(files), basename(urls))
})

test_that("Missing file", {
    expect_error(
        localOrRemoteFile("XXX.csv"),
        "is_existing_file :"
    )
    expect_warning(
        localOrRemoteFile("XXX.csv", severity = "warning"),
        "is_existing_file :"
    )
    expect_identical(
        suppressWarnings(
            localOrRemoteFile("XXX.csv", severity = "warning")
        ),
        NULL
    )
})
