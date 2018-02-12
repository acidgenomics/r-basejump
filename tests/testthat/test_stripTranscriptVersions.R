context("stripTranscriptVersions")

test_that("stripTranscriptVersions", {
    expect_identical(
        stripTranscriptVersions("ENSMUST00000119854.7"),
        "ENSMUST00000119854"
    )
    expect_error(
        stripTranscriptVersions("xxx.1"),
        "Failed to sanitize Ensembl transcript identifier"
    )
})
