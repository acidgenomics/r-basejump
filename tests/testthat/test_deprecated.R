context("Deprecated")

test_that("v0.0.24", {
    expect_warning(
        metadataTable(),
        "Use 'sampleMetadata' instead."
    )
})

test_that("v0.0.25", {
    expect_warning(
        pct(0.5),
        "Use 'scales::percent' instead."
    )
})

test_that("v0.1.0", {
    expect_warning(
        sampleDirs(),
        "'sampleDirs' is deprecated."
    )
})
