context("GFF Functions")



# makeGene2symbolFromGFF =======================================================
test_that("makeGene2symbolFromGFF : Minimal GTF", {
    x <- makeGene2symbolFromGFF("example.gtf")
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

# FIXME This is broken
test_that("makeGene2symbolFromGFF : Minimal GFF3", {
    x <- makeGene2symbolFromGFF("example.gff3")
})



# makeGRangesFromGFF ===========================================================
test_that("makeGRangesFromGFF : Minimal GTF", {
    # Genes
    x <- makeGRangesFromGFF("example.gtf", format = "genes")
    expect_s4_class(x, "GRanges")
    expect_identical(length(x), 17L)
    expect_identical(names(x)[[1L]], "ENSMUSG00000025900")
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

    # Transcripts
    x <- makeGRangesFromGFF("example.gtf", format = "transcripts")
    expect_s4_class(x, "GRanges")
    expect_identical(length(x), 20L)
    expect_identical(names(x)[[1L]], "ENSMUST00000070533")
    expect_identical(
        lapply(mcols(x), class),
        list(
            broadClass = "factor",
            ccdsID = "factor",
            geneBiotype = "factor",
            geneID = "factor",
            geneName = "factor",
            geneSource = "factor",
            geneVersion = "factor",
            source = "factor",
            tag = "factor",
            transcriptBiotype = "factor",
            transcriptID = "character",
            transcriptName = "character",
            transcriptSource = "factor",
            transcriptSupportLevel = "factor",
            transcriptVersion = "factor",
            type = "factor"
        )
    )
})

test_that("makeGRangesFromGFF : Minimal GFF3", {
    # Genes
    x <- makeGRangesFromGFF("example.gff3", format = "genes")
    expect_s4_class(x, "GRanges")
    expect_identical(length(x), 20L)
    expect_identical(names(x)[[1L]], "ENSMUSG00000025900")
    expect_identical(
        lapply(mcols(x), class),
        list(
            broadClass = "factor",
            description = "character",
            geneBiotype = "factor",
            geneID = "character",
            geneName = "character",
            havanaGene = "factor",
            havanaVersion = "factor",
            logicName = "factor",
            source = "factor",
            type = "factor",
            version = "factor"
        )
    )

    # Transcripts
    x <- makeGRangesFromGFF("example.gff3", format = "transcripts")
    expect_s4_class(x, "GRanges")
    expect_identical(length(x), 26L)
    expect_identical(names(x)[[1L]], "ENSMUST00000027032")
    expect_identical(
        lapply(mcols(x), class),
        list(
            broadClass = "factor",
            ccdsID = "factor",
            geneBiotype = "factor",
            geneID = "factor",
            geneName = "factor",
            havanaTranscript = "factor",
            havanaVersion = "factor",
            source = "factor",
            tag = "factor",
            transcriptBiotype = "factor",
            transcriptID = "character",
            transcriptName = "character",
            transcriptSupportLevel = "factor",
            type = "factor",
            version = "factor"
        )
    )
})



# makeTx2geneFromGFF ===========================================================
test_that("makeTx2geneFromGFF : Minimal GTF", {
    x <- makeTx2geneFromGFF("example.gtf")
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
})

# FIXME This is broken
test_that("makeTx2geneFromGFF : Minimal GFF3", {
    x <- makeTx2geneFromGFF("example.gff3")
})
