context("GFF Functions")



# makeGene2symbolFromGFF =======================================================
test_that("makeGene2symbolFromGFF : Mus musculus", {
    x <- makeGene2symbolFromGFF("mmusculus.gtf")
    expect_identical(dim(x), c(17L, 2L))
    expect_identical(
        head(x, 2L),
        data.frame(
            "geneID" = c("ENSMUSG00000025900", "ENSMUSG00000051951"),
            "geneName" = c("Rp1", "Xkr4"),
            row.names = c("ENSMUSG00000025900", "ENSMUSG00000051951"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("makeGene2symbolFromGFF : Drosophila melanogaster", {
    x <- makeGene2symbolFromGFF("dmelanogaster.gtf")
    expect_identical(dim(x), c(5L, 2L))
    expect_identical(
        head(x, 2L),
        data.frame(
            "geneID" = c("FBgn0031081", "FBgn0031085"),
            "geneName" = c("Nep3", "CG9570"),
            row.names = c("FBgn0031081", "FBgn0031085"),
            stringsAsFactors = FALSE
        )
    )
})



# makeGRangesFromGFF ===========================================================
test_that("makeGRangesFromGFF : genes", {
    x <- makeGRangesFromGFF("mmusculus.gtf", format = "genes")
    expect_identical(length(x), 17L)
    expect_identical(names(x)[[1L]], "ENSMUSG00000025900")
    # FIXME Need to update internal code to match
    expect_identical(
        lapply(mcols(x), class),
        list(
            broadClass = "factor",
            geneBiotype = "factor",
            geneID = "character",
            geneName = "character",
            geneSource = "factor",
            geneVersion = "factor",
            source = "factor",
            type = "factor"
        )
    )
})

test_that("makeGRangesFromGFF : transcripts", {
    # Expected warning about `phase` metadata column
    x <- suppressWarnings(
        makeGRangesFromGFF("mmusculus.gtf", format = "transcripts")
    )
    expect_identical(length(x), 20L)
    expect_identical(names(x)[[1L]], "ENSMUST00000070533")
    expect_identical(
        colnames(mcols(x)),
        c(
            "transcriptID",
            "transcriptName",
            "transcriptBiotype",
            "geneID",
            "geneName",
            "geneBiotype",
            "geneVersion",
            "geneSource",
            "transcriptVersion",
            "transcriptSource",
            "transcriptSupportLevel",
            "broadClass"
        )
    )
})



# makeTx2geneFromGFF ===========================================================
test_that("makeTx2geneFromGFF : Drosophila melanogaster", {
    x <- makeTx2geneFromGFF("dmelanogaster.gtf")
    expect_identical(dim(x), c(7L, 2L))
    expect_identical(
        head(x, 2L),
        data.frame(
            transcriptID = c("FBtr0070000", "FBtr0070001"),
            geneID = c("FBgn0031081", "FBgn0052826"),
            row.names = c("FBtr0070000", "FBtr0070001"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("makeTx2geneFromGFF : Mus musculus", {
    x <- makeTx2geneFromGFF("mmusculus.gtf")
    expect_identical(dim(x), c(20L, 2L))
    expect_identical(
        head(x, 2L),
        data.frame(
            transcriptID = c("ENSMUST00000070533", "ENSMUST00000082908"),
            geneID = c("ENSMUSG00000051951", "ENSMUSG00000064842"),
            row.names = c("ENSMUST00000070533", "ENSMUST00000082908"),
            stringsAsFactors = FALSE
        )
    )
    expect_message(
        makeTx2geneFromGFF("mmusculus.gtf"),
        "tx2gene mappings: 20 transcripts, 17 genes"
    )
})
