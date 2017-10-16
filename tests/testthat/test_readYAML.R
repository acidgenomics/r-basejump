context("readYAML")

test_that("readYAML", {
    yaml <- file.path(testDataURL, "project-summary.yaml") %>%
        readYAML(quiet = TRUE)
    expect_equal(
        class(yaml),
        "list"
    )
    expect_equal(
        names(yaml),
        c("date", "upload", "bcbio_system", "samples")
    )

    # Check '.yml' file support
    yaml <- file.path(
        "https://raw.githubusercontent.com",
        "steinbaugh",
        "basejump",
        "master",
        ".travis.yml") %>%
        readYAML(quiet = TRUE)
    expect_true("language" %in% names(yaml))

    # Unsupported file type
    expect_error(
        readYAML(file.path(testDataURL, "mtcars.csv"), quiet = TRUE),
        "YAML file must have '.yaml' or '.yml' extension"
    )

    # Warn and return `NULL` on missing file
    expect_warning(
        readYAML("foobar.yaml", quiet = TRUE),
        "foobar.yaml file missing"
    )
    expect_equal(
        readYAML("foobar.yaml", quiet = TRUE),
        NULL
    )
})
