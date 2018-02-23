context("convertTranscriptsToGenes")

test_that("Character", {
    expect_identical(
        convertTranscriptsToGenes(
            c("ENSMUST00000000001", "ENSMUST00000000003"),
            release = 88L),
        c("ENSMUST00000000001" = "ENSMUSG00000000001",
          "ENSMUST00000000003" = "ENSMUSG00000000003")
    )
    expect_error(
        convertTranscriptsToGenes(
            c("ENSMUST00000000000",
              "ENSMUST00000000001"),
            release = 88L),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", NA)),
        "is_non_missing_nor_empty_character"
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", "")),
        "is_non_missing_nor_empty_character"
    )
})

test_that("Matrix", {
    mat <- matrix(
        data = seq(1L:8L),
        byrow = TRUE,
        nrow = 4L,
        ncol = 2L,
        dimnames = list(
            c("ENSMUST00000000000",
              "ENSMUST00000000001",
              "ENSMUST00000000003",
              "ENSMUST00000114041"),
            c("sample1", "sample2")
        )
    )
    expect_error(
        convertTranscriptsToGenes(mat, release = 88L),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_identical(
        mat[2L:4L, ] %>%
            convertTranscriptsToGenes() %>%
            rownames(),
        c("ENSMUST00000000001" = "ENSMUSG00000000001",
          "ENSMUST00000000003" = "ENSMUSG00000000003",
          "ENSMUST00000114041" = "ENSMUSG00000000003")
    )
})
