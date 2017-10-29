context("readYAML")

test_that("bcbio project summary", {
    yaml <- "http://basejump.seq.cloud/project-summary.yaml" %>%
        readYAML(quiet = TRUE)
    expect_equal(
        class(yaml),
        "list"
    )
    expect_equal(
        names(yaml),
        c("date", "upload", "bcbio_system", "samples")
    )
})

test_that("'.yml' file support", {
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

test_that("unsupported file type", {
    expect_error(
        readYAML("http://basejump.seq.cloud/mtcars.csv", quiet = TRUE),
        "YAML file must have '.yaml' or '.yml' extension"
    )
})

# Warn and return `NULL` on missing file
test_that("missing file", {
    yaml <- suppressWarnings(
        readYAML("foobar.yaml", quiet = TRUE)
    )
    expect_warning(
        readYAML("foobar.yaml", quiet = TRUE),
        "foobar.yaml file missing"
    )
    expect_equal(yaml, NULL)
})
