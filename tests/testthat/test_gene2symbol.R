context("gene2symbol")

test_that("character", {
    g2s <- gene2symbol(
        c("ENSMUSG00000000001",
          "ENSMUSG00000000003"),
        release = 88L,
        quiet = TRUE)
    expect_equal(
        g2s,
        c(ENSMUSG00000000001 = "Gnai3",
          ENSMUSG00000000003 = "Pbsn")
    )
})

test_that("matrix", {
    mat <- matrix(
        data = seq(1L:4L),
        byrow = TRUE,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(c("ENSMUSG00000000001",
                          "ENSMUSG00000000003"),
                        c("sample1", "sample2"))
    )
    g2s <- gene2symbol(
        mat,
        release = 88L,
        quiet = TRUE)
    expect_equal(
        rownames(g2s),
        c(ENSMUSG00000000001 = "Gnai3",
          ENSMUSG00000000003 = "Pbsn")
    )
})

# Specify organism (to handle FASTA spike-ins (e.g. EGFP)
test_that("fasta spike-in support", {
    vec <- c("EGFP", "ENSMUSG00000000001")
    expect_error(
        gene2symbol(vec),
        "Failed to detect supported organism"
    )
    expect_equal(
        gene2symbol(
            vec,
            organism = "Mus musculus",
            release = 88L,
            quiet = TRUE),
        c(EGFP = "EGFP",
          ENSMUSG00000000001 = "Gnai3")
    )
    expect_warning(
        gene2symbol(
            vec,
            organism = "Mus musculus",
            release = 88L,
            quiet = TRUE),
        "Failed to match all gene IDs to symbols: EGFP"
    )
})

test_that("invalid identifiers", {
    expect_warning(
        gene2symbol(
            c("ENSMUSG00000000000",
              "ENSMUSG00000000001"),
            release = 88L,
            quiet = TRUE),
        "Failed to match all gene IDs to symbols"
    )
    expect_warning(
        gene2symbol(
            c("ENSMUSG00000000001",
              "ENSMUSG00000000001"),
            release = 88L,
            quiet = TRUE),
        "Duplicate gene identifiers detected"
    )
    expect_error(
        gene2symbol(c("ENSMUSG00000000001", NA)),
        "NA identifier detected"
    )
    expect_error(
        gene2symbol(c("ENSMUSG00000000001", "")),
        "Empty string identifier detected"
    )
})

test_that("prevent genome build pass-in", {
    expect_error(
        gene2symbol("mm10"),
        "gene2symbol conversion requires > 1 identifier"
    )
})
