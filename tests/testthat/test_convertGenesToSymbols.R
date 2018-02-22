context("convertGenesToSymbols")

test_that("Character", {
    g2s <- convertGenesToSymbols(
        c("ENSMUSG00000000001",
          "ENSMUSG00000000003"),
        release = 88L)
    expect_identical(
        g2s,
        c(ENSMUSG00000000001 = "Gnai3",
          ENSMUSG00000000003 = "Pbsn")
    )
})

test_that("Matrix", {
    mat <- matrix(
        data = seq(1L:4L),
        byrow = TRUE,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("ENSMUSG00000000001",
              "ENSMUSG00000000003"),
            c("sample1", "sample2")
        )
    )
    g2s <- convertGenesToSymbols(mat, release = 88L)
    expect_identical(
        rownames(g2s),
        c(ENSMUSG00000000001 = "Gnai3",
          ENSMUSG00000000003 = "Pbsn")
    )
})

test_that("Unique symbol mode", {
    gene2symbol <- gene2symbol("Homo sapiens", uniqueSymbol = FALSE)
    expect_true(any(duplicated(gene2symbol[["symbol"]])))
    dupes <- gene2symbol %>%
        .[which(duplicated(.[["symbol"]])), "ensgene", drop = TRUE]
    map <- convertGenesToSymbols(
        dupes,
        gene2symbol = gene2symbol,
        uniqueSymbol = TRUE)
    expect_false(any(duplicated(map)))
})

# Specify organism (to handle FASTA spike-ins (e.g. EGFP)
test_that("FASTA spike-in support", {
    vec <- c("EGFP", "ENSMUSG00000000001")
    g2s <- suppressWarnings(
        convertGenesToSymbols(
            vec,
            organism = "Mus musculus",
            release = 88L)
    )
    expect_identical(
        g2s,
        c(EGFP = "EGFP",
          ENSMUSG00000000001 = "Gnai3")
    )
    expect_warning(
        convertGenesToSymbols(
            vec,
            organism = "Mus musculus",
            release = 88L),
        "Failed to match all genes to symbols: EGFP"
    )
})

test_that("Invalid identifiers", {
    expect_warning(
        convertGenesToSymbols(
            c("ENSMUSG00000000000",
              "ENSMUSG00000000001"),
            release = 88L),
        "Failed to match all genes to symbols: ENSMUSG00000000000"
    )
    expect_error(
        convertGenesToSymbols(
            c("ENSMUSG00000000001",
              "ENSMUSG00000000001"),
            release = 88L),
        "has_no_duplicates"
    )
    expect_error(
        convertGenesToSymbols(c("ENSMUSG00000000001", NA)),
        "is_non_missing_nor_empty_character"
    )
    expect_error(
        convertGenesToSymbols(c("ENSMUSG00000000001", "")),
        "is_non_missing_nor_empty_character"
    )
})
