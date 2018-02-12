context("readYAML")

test_that("bcbio project summary", {
    url <- file.path(
        "http://bcbiobase.seq.cloud",
        "bcbio",
        "project-summary.yaml")
    yaml <- readYAML(url, quiet = TRUE)
    expect_identical(
        class(yaml),
        "list"
    )
    expect_identical(
        names(yaml),
        c("date", "upload", "bcbio_system", "samples")
    )
    # Check for message
    expect_message(
        readYAML(url),
        "Reading project-summary.yaml"
    )
})

test_that("`.yml` file support", {
    # Use the package `.travis.yml` configuration as an example
    yaml <- readYAML(
        file.path(
            "https://raw.githubusercontent.com",
            "steinbaugh",
            "basejump",
            "master",
            ".travis.yml"),
        quiet = TRUE)
    expect_true("language" %in% names(yaml))
})

test_that("Unsupported file type", {
    expect_error(
        readYAML("http://basejump.seq.cloud/mtcars.csv", quiet = TRUE),
        "is_matching_regex : object"
    )
})

test_that("Missing file", {
    expect_error(
        readYAML("foobar.yaml"),
        "is_existing_file"
    )
})
