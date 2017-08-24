context("Gene Annotation Utilities")

test_that("annotable", {
    # Human
    anno <- annotable("grch38")
    expect_equal(
        dim(anno),
        c(63898L, 9L))
    expect_equal(
        rownames(anno)[[1L]],
        "ENSG00000000003")

    # Mouse
    anno <- annotable("grcm38")
    expect_equal(
        dim(anno),
        c(52386L, 9L))
    expect_equal(
        rownames(anno)[[1L]],
        "ENSMUSG00000000001")

    # Genome build aliases
    expect_equal(
        annotable("grch37"),
        annotable("hg19"))
    expect_equal(
        annotable("grch38"),
        annotable("hg38"))
    expect_equal(
        annotable("grcm38"),
        annotable("mm10"))
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

    # Prevent accidental `genomeBuild` pass in
    expect_error(
        gene2symbol("mm10"),
        "gene2symbol conversion requires > 1 identifier")
})



test_that("gene2symbolFromGTF", {
    # Mouse
    mm <- file.path(testDataURL, "mmusculus.gtf") %>%
        gene2symbolFromGTF
    expect_equal(
        dim(mm),
        c(17L, 2L))
    expect_equal(
        head(mm, 2L),
        data.frame(
            ensgene = c("ENSMUSG00000025900",
                        "ENSMUSG00000051951"),
            symbol = c("Rp1",
                       "Xkr4"),
            row.names = c("ENSMUSG00000025900",
                          "ENSMUSG00000051951")))

    # Fruitfly
    dm <- file.path(testDataURL, "dmelanogaster.gtf") %>%
        gene2symbolFromGTF
    expect_equal(
        dim(dm),
        c(5L, 2L))
    expect_equal(
        head(dm, 2L),
        data.frame(
            ensgene = c("FBgn0031081",
                        "FBgn0031085"),
            symbol = c("Nep3",
                       "CG9570"),
            row.names = c("FBgn0031081",
                          "FBgn0031085")))
})



test_that("readGTF", {
    mm <- file.path(testDataURL, "mmusculus.gtf")
    # Check for 9 columns
    expect_equal(
        readGTF(mm) %>%
            dim %>%
            .[[2L]],
        9L)

    dm <- file.path(testDataURL, "dmelanogaster.gtf")
    # Check for 9 columns
    expect_equal(
        readGTF(dm) %>%
            dim %>%
            .[[2L]],
        9L)

    # Bad URL
    expect_error(
        file.path(testDataURL, "mtcars.rda") %>%
            readGTF,
        "GTF file failed to load. Check path.")
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

    # Prevent accidental `genomeBuild` pass in
    expect_error(
        tx2gene("mm10"),
        "tx2gene conversion requires > 1 identifier")
})



test_that("tx2geneFromGTF", {
    # Mouse
    mm <- file.path(testDataURL, "mmusculus.gtf") %>%
        tx2geneFromGTF
    expect_equal(
        dim(mm),
        c(20L, 2L))
    expect_equal(
        head(mm, 2L),
        data.frame(
            enstxp = c("ENSMUST00000070533",
                       "ENSMUST00000082908"),
            ensgene = c("ENSMUSG00000051951",
                        "ENSMUSG00000064842"),
            row.names = c("ENSMUST00000070533",
                          "ENSMUST00000082908")))

    # Fruitfly
    dm <- file.path(testDataURL, "dmelanogaster.gtf") %>%
        tx2geneFromGTF
    expect_equal(
        dim(dm),
        c(7L, 2L))
    expect_equal(
        head(dm, 2L),
        data.frame(
            enstxp = c("FBtr0070000",
                       "FBtr0070001"),
            ensgene = c("FBgn0031081",
                        "FBgn0052826"),
            row.names = c("FBtr0070000",
                          "FBtr0070001")))
})
