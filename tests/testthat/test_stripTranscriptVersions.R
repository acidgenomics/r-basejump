context("stripTranscriptVersions")

test_that("stripTranscriptVersions", {
    expect_identical(
        stripTranscriptVersions("ENSMUST00000119854.7"),
        "ENSMUST00000119854"
    )
    # Require detectinon of Ensembl transcript (ENS*T)
    expect_error(
        stripTranscriptVersions("EGFP.1"),
        "is_matching_regex"
    )
    # Theoretical spike-in containing a transcript version
    expect_identical(
        stripTranscriptVersions(c("ENSMUST00000119854.7", "EGFP.1")),
        c("ENSMUST00000119854", "EGFP")
    )
})
