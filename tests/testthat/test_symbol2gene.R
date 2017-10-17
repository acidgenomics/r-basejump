context("symbol2gene")

test_that("symbol2gene", {
    # character
    expect_equal(
        symbol2gene(
            c("Gnai3", "Pbsn"),
            organism = "Mus musculus",
            release = 88L,
            quiet = TRUE),
        c(Gnai3 = "ENSMUSG00000000001",
          Pbsn = "ENSMUSG00000000003")
    )
    expect_warning(
        symbol2gene(
            c("Gnai3", "Gnai3"),
            organism = "Mus musculus",
            release = 88L,
            quiet = TRUE),
        "Duplicate gene symbols detected"
    )
    expect_error(
        symbol2gene(
            "Gnai3",
            organism = "Mus musculus"),
        "symbol2gene conversion requires > 1 identifier"
    )
    expect_error(
        symbol2gene(
            c("Gnai3", "Pbsn", ""),
            organism = "Mus musculus"),
        "Empty string identifier detected"
    )
    expect_error(
        symbol2gene(
            c("Gnai3", "Pbsn", NA),
            organism = "Mus musculus"),
        "NA identifier detected"
    )

    # Identifier mismatch
    expect_equal(
        suppressWarnings(symbol2gene(
            c("Gnai3", "Pbsn", "XXX"),
            organism = "Mus musculus",
            release = 88L,
            quiet = TRUE)),
        c(Gnai3 = "ENSMUSG00000000001",
          Pbsn = "ENSMUSG00000000003",
          XXX = "XXX")
    )
    expect_warning(
        symbol2gene(
            c("Gnai3", "Pbsn", "XXX"),
            organism = "Mus musculus",
            release = 88L,
            quiet = TRUE),
        "Failed to match all gene symbols to IDs: XXX"
    )

    # matrix
    expect_equal(
        matrix(
            data = seq(1L:4L),
            byrow = TRUE,
            nrow = 2L,
            ncol = 2L,
            dimnames = list(c("Gnai3", "Pbsn"),
                            c("sample1", "sample2"))) %>%
            symbol2gene(
                organism = "Mus musculus",
                release = 88L,
                quiet = TRUE) %>%
            dimnames() %>%
            .[[1L]],
        c(Gnai3 = "ENSMUSG00000000001",
          Pbsn = "ENSMUSG00000000003")
    )
})
