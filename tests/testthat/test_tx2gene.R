context("tx2gene")

test_that("tx2gene", {
    # character
    expect_equal(
        tx2gene(
            c("ENSMUST00000000001",
              "ENSMUST00000000003"),
            release = 88L,
            quiet = TRUE),
        c(ENSMUST00000000001 = "ENSMUSG00000000001",
          ENSMUST00000000003 = "ENSMUSG00000000003")
    )
    expect_error(
        tx2gene(
            c("ENSMUST00000000000",
              "ENSMUST00000000001"),
            release = 88L,
            quiet = TRUE),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_error(
        tx2gene(c("ENSMUSG00000000001", NA)),
        "NA identifier detected"
    )
    expect_error(
        tx2gene(c("ENSMUSG00000000001", "")),
        "Empty string identifier detected"
    )

    # matrix
    mat <- matrix(
        data = seq(1L:8L),
        byrow = TRUE,
        nrow = 4L,
        ncol = 2L,
        dimnames = list(c("ENSMUST00000000000",
                          "ENSMUST00000000001",
                          "ENSMUST00000000003",
                          "ENSMUST00000114041"),
                        c("sample1", "sample2"))
    )
    expect_error(
        tx2gene(
            mat,
            release = 88L,
            quiet = TRUE),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_equal(
        mat[2L:4L, ] %>%
            tx2gene(quiet = TRUE) %>%
            dimnames() %>%
            .[[1L]],
        c(ENSMUST00000000001 = "ENSMUSG00000000001",
          ENSMUST00000000003 = "ENSMUSG00000000003",
          ENSMUST00000114041 = "ENSMUSG00000000003")
    )

    # Prevent accidental `genomeBuild` pass in
    expect_error(
        tx2gene("mm10"),
        "tx2gene conversion requires > 1 identifier"
    )
})
