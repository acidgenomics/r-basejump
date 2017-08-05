context("Annotables Utilities")

test_that("ensemblVersion", {
    expect_equal(
        grepl("^Ensembl Genes ", ensemblVersion()),
        TRUE)
    expect_error(ensemblVersion("x"))
})



test_that("gene2entrez", {
    expect_equal(
        colnames(gene2entrez("grch38")),
        c("ensgene", "entrez"))
})



test_that("gene2symbol", {
    # character
    expect_equal(
        gene2symbol("ENSMUSG00000000001"),
        c(ENSMUSG00000000001 = "Gnai3"))
    expect_warning(
        gene2symbol("ENSMUSG00000000000"),
        "Failed to match all gene IDs to symbols")
    expect_error(gene2symbol(c("ENSMUSG00000000001", NA)))
    expect_error(gene2symbol(c("ENSMUSG00000000001", "")))

    # matrix
    mat <- matrix(
        data = seq(1L:6L),
        byrow = TRUE,
        nrow = 3L,
        ncol = 2L,
        dimnames = list(c("ENSMUSG00000000000",
                          "ENSMUSG00000000001",
                          "ENSMUSG00000000003"),
                        c("sample1", "sample2")))
    expect_warning(
        gene2symbol(mat),
        "Failed to match all gene IDs to symbols")
    expect_equal(
        mat[2L:3L, ] %>%
            gene2symbol %>%
            dimnames %>%
            .[[1L]],
        c(ENSMUSG00000000001 = "Gnai3",
          ENSMUSG00000000003 = "Pbsn"))
})



test_that("tx2gene", {
    # character
    expect_equal(
        tx2gene("ENSMUST00000000001"),
        c(ENSMUST00000000001 = "ENSMUSG00000000001"))
    expect_error(
        tx2gene(c("ENSMUST00000000000", "ENSMUST00000000001")))
    expect_error(tx2gene(c("ENSMUSG00000000001", NA)))
    expect_error(tx2gene(c("ENSMUSG00000000001", "")))

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
                        c("sample1", "sample2")))
    expect_error(tx2gene(mat))
    expect_equal(
        mat[2L:4L, ] %>%
            tx2gene %>%
            dimnames %>%
            .[[1L]],
        c(ENSMUST00000000001 = "ENSMUSG00000000001",
          ENSMUST00000000003 = "ENSMUSG00000000003",
          ENSMUST00000114041 = "ENSMUSG00000000003"))
})
