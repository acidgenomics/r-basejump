context("Gene Annotation Utilities")

release <- 88L  # current version on bioc-release

test_that("annotable", {
    # Human
    anno <- annotable("Homo sapiens", release = release)
    expect_equal(
        dim(anno),
        c(64592L, 5L))
    expect_equal(
        rownames(anno)[1L:5L],
        c("ENSG00000000003",
          "ENSG00000000005",
          "ENSG00000000419",
          "ENSG00000000457",
          "ENSG00000000460"))

    # Mouse
    anno <- annotable("Mus musculus", release = release)
    expect_equal(
        dim(anno),
        c(51158L, 5L))
    expect_equal(
        rownames(anno)[1L:5L],
        c("ENSMUSG00000000001",
          "ENSMUSG00000000003",
          "ENSMUSG00000000028",
          "ENSMUSG00000000031",
          "ENSMUSG00000000037"))

    # Bad input
    expect_error(
        annotable(c("human", "mouse")),
        "Object must be a string")
    expect_error(
        annotable("Homo sapiens", format = "XXX"),
        "Unsupported format")
    expect_error(
        annotable("XXX"),
        "Failed to detect supported organism")
})



test_that("gene2symbol", {
    # character
    expect_equal(
        gene2symbol(
            c("ENSMUSG00000000001", "ENSMUSG00000000003"),
            release = release),
        c(ENSMUSG00000000001 = "Gnai3",
          ENSMUSG00000000003 = "Pbsn"))
    expect_warning(
        gene2symbol(
            c("ENSMUSG00000000000", "ENSMUSG00000000001"),
            release = release),
        "Failed to match all gene IDs to symbols")
    expect_warning(
        gene2symbol(
            c("ENSMUSG00000000001", "ENSMUSG00000000001"),
            release = release),
        "Duplicate gene identifiers detected")

    expect_error(
        gene2symbol(c("ENSMUSG00000000001", NA)),
        "NA identifier detected")
    expect_error(
        gene2symbol(c("ENSMUSG00000000001", "")),
        "Empty string identifier detected")

    # Specify organism (to handle FASTA spike-ins (e.g. EGFP)
    vec <- c("EGFP", "ENSMUSG00000000001")
    expect_error(
        gene2symbol(vec),
        "Failed to detect supported organism")
    expect_equal(
        gene2symbol(vec,
                    organism = "Mus musculus",
                    release = release),
        c(EGFP = "EGFP",
          ENSMUSG00000000001 = "Gnai3"))
    expect_warning(
        gene2symbol(vec,
                    organism = "Mus musculus",
                    release = release),
        "Failed to match all gene IDs to symbols: EGFP")

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
    expect_equal(
        mat[2L:3L, ] %>%
            gene2symbol(release = release) %>%
            dimnames %>%
            .[[1L]],
        c(ENSMUSG00000000001 = "Gnai3",
          ENSMUSG00000000003 = "Pbsn"))
    expect_warning(
        gene2symbol(mat, release = release),
        "Failed to match all gene IDs to symbols")

    # Prevent accidental `genomeBuild` pass in
    expect_error(
        gene2symbol("mm10"),
        "gene2symbol conversion requires > 1 identifier")
})



test_that("gene2symbolFromGFF", {
    # Mouse
    file <- file.path(testDataURL, "mmusculus.gtf")
    mm <- gene2symbolFromGFF(file.path(file))
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
    # Test GFF data.frame input
    gtf <- readGFF(file)
    expect_equal(
        gene2symbolFromGFF(gtf),
        mm)

    # Fruitfly
    dm <- file.path(testDataURL, "dmelanogaster.gtf") %>%
        gene2symbolFromGFF
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



test_that("readGFF", {
    mm <- file.path(testDataURL, "mmusculus.gtf")
    # Check for 9 columns
    expect_equal(
        readGFF(mm) %>%
            dim %>%
            .[[2L]],
        9L)

    dm <- file.path(testDataURL, "dmelanogaster.gtf")
    # Check for 9 columns
    expect_equal(
        readGFF(dm) %>%
            dim %>%
            .[[2L]],
        9L)

    # Bad URL
    expect_error(
        readGFF(file.path(testDataURL, "mtcars.rda")),
        "GFF file failed to load")

    # Bad GFF file
    expect_error(
        readGFF(file.path(testDataURL, "mtcars.tsv")),
        "GFF file failed to load")
})



test_that("symbol2gene", {
    # character
    expect_equal(
        symbol2gene(c("Gnai3", "Pbsn"),
                    organism = "Mus musculus",
                    release = release),
        c(Gnai3 = "ENSMUSG00000000001",
          Pbsn = "ENSMUSG00000000003"))
    expect_warning(
        symbol2gene(c("Gnai3", "Gnai3"),
                    organism = "Mus musculus",
                    release = release),
        "Duplicate gene symbols detected")

    expect_error(
        symbol2gene("Gnai3", organism = "Mus musculus"),
        "symbol2gene conversion requires > 1 identifier")
    expect_error(
        symbol2gene(c("Gnai3", "Pbsn", ""), organism = "Mus musculus"),
        "Empty string identifier detected")
    expect_error(
        symbol2gene(c("Gnai3", "Pbsn", NA), organism = "Mus musculus"),
        "NA identifier detected")

    # Identifier mismatch
    expect_equal(
        symbol2gene(
            c("Gnai3", "Pbsn", "XXX"),
            organism = "Mus musculus",
            release = release),
        c(Gnai3 = "ENSMUSG00000000001",
          Pbsn = "ENSMUSG00000000003",
          XXX = "XXX"))
    expect_warning(
        symbol2gene(
            c("Gnai3", "Pbsn", "XXX"),
            organism = "Mus musculus",
            release = release),
        "Failed to match all gene symbols to IDs: XXX")

    # matrix
    expect_equal(
        matrix(
            data = seq(1L:4L),
            byrow = TRUE,
            nrow = 2L,
            ncol = 2L,
            dimnames = list(c("Gnai3", "Pbsn"),
                            c("sample1", "sample2"))) %>%
            symbol2gene(organism = "Mus musculus", release = release) %>%
            dimnames %>%
            .[[1L]],
        c(Gnai3 = "ENSMUSG00000000001",
          Pbsn = "ENSMUSG00000000003"))
})



test_that("tx2gene", {
    # character
    expect_equal(
        tx2gene(
            c("ENSMUST00000000001", "ENSMUST00000000003"),
            release = release),
        c(ENSMUST00000000001 = "ENSMUSG00000000001",
          ENSMUST00000000003 = "ENSMUSG00000000003"))
    expect_error(
        tx2gene(
            c("ENSMUST00000000000", "ENSMUST00000000001"),
            release = release),
        "Unmatched transcripts present. Try using a GFF file instead.")
    expect_error(
        tx2gene(c("ENSMUSG00000000001", NA)),
        "NA identifier detected")
    expect_error(
        tx2gene(c("ENSMUSG00000000001", "")),
        "Empty string identifier detected")

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
    expect_error(
        tx2gene(mat, release = release),
        "Unmatched transcripts present. Try using a GFF file instead.")
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



test_that("tx2geneFromGFF", {
    # Mouse
    file <- file.path(testDataURL, "mmusculus.gtf")
    mm <- tx2geneFromGFF(file.path(file))
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
    # Test GFF data.frame input
    gtf <- readGFF(file)
    expect_equal(
        tx2geneFromGFF(gtf),
        mm)

    # Fruitfly
    dm <- file.path(testDataURL, "dmelanogaster.gtf") %>%
        tx2geneFromGFF
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

    # bad data.frame
    expect_error(
        tx2geneFromGFF(mtcars),
        "GFF object must be data.frame with 9 columns")
})
