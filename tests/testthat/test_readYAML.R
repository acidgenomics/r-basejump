context("readYAML")

test_that("bcbio project summary", {
    yaml <- readYAML("summary.yaml")
    expect_identical(
        class(yaml),
        "list"
    )
    expect_identical(
        names(yaml),
        c("date", "upload", "bcbio_system", "samples")
    )
})

test_that("Unsupported file type", {
    expect_error(
        readYAML("mtcars.csv"),
        "is_matching_regex : object"
    )
})

test_that("Missing file", {
    expect_error(
        readYAML("foobar.yaml"),
        "is_existing_file"
    )
})
