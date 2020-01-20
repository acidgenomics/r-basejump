context("convertUCSCBuildToEnsembl")

with_parameters_test_that(
    "convertUCSCBuildToEnsembl", {
        expect_identical(
            object = unname(convertUCSCBuildToEnsembl(object)),
            expected = expected
        )
    },
    object = list(
        "hg19",
        "hg38",
        "mm10"
    ),
    expected = list(
        "GRCh37",
        "GRCh38",
        "GRCm38"
    )
)

test_that("Expected failure", {
    expect_error(
        object = convertUCSCBuildToEnsembl("XXX"),
        regexp = "Failed to match UCSC"
    )
})
