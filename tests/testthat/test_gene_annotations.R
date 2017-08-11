context("Gene Annotation Utilities")

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



test_that("gene2symbolFromGTF", {
    # Mouse
    mm <- file.path("http://steinbaugh.com",
                    "basejump",
                    "tests",
                    "mmusculus.gtf") %>%
        gene2symbolFromGTF
    expect_equal(
        dim(mm),
        c(17L, 2L))
    expect_equal(
        mm %>%
            head(2L),
        data.frame(
            ensgene = c("ENSMUSG00000025900",
                        "ENSMUSG00000051951"),
            symbol = c("Rp1",
                       "Xkr4"),
            row.names = c("ENSMUSG00000025900",
                          "ENSMUSG00000051951")))

    # Fruitfly
    dm <- file.path("http://steinbaugh.com",
                    "basejump",
                    "tests",
                    "dmelanogaster.gtf") %>%
        gene2symbolFromGTF
    expect_equal(
        dim(dm),
        c(5L, 2L))
    expect_equal(
        dm %>%
            head(2L),
        data.frame(
            ensgene = c("FBgn0031081",
                        "FBgn0031085"),
            symbol = c("Nep3",
                       "CG9570"),
            row.names = c("FBgn0031081",
                          "FBgn0031085")))
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



test_that("tx2geneFromGTF", {
    # Mouse
    mm <- file.path("http://steinbaugh.com",
                    "basejump",
                    "tests",
                    "mmusculus.gtf") %>%
        tx2geneFromGTF
    expect_equal(
        dim(mm),
        c(20L, 2L))
    expect_equal(
        mm[1L, , drop = FALSE],
        data.frame(
            enstxp = "ENSMUST00000070533",
            ensgene = "ENSMUSG00000051951",
            row.names = "ENSMUST00000070533"))

    # Fruitfly
    dm <- file.path("http://steinbaugh.com",
                    "basejump",
                    "tests",
                    "dmelanogaster.gtf") %>%
        tx2geneFromGTF
    expect_equal(
        dim(dm),
        c(7L, 2L))
    expect_equal(
        dm[1L, , drop = FALSE],
        data.frame(
            enstxp = "FBtr0070000",
            ensgene = "FBgn0031081",
            row.names = "FBtr0070000"))
})
