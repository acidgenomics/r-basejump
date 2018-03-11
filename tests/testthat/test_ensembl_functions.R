context("Ensembl Functions")

# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols : character", {
    x <- convertGenesToSymbols(
        c("ENSMUSG00000000001", "ENSMUSG00000000003"),
        release = ensemblRelease
    )
    expect_identical(
        x,
        c(
            "ENSMUSG00000000001" = "Gnai3",
            "ENSMUSG00000000003" = "Pbsn"
        )
    )
})

test_that("convertGenesToSymbols : matrix", {
    mat <- matrix(
        data = seq(1L:4L),
        byrow = TRUE,
        nrow = 2L,
        ncol = 2L,
        dimnames = list(
            c("ENSMUSG00000000001", "ENSMUSG00000000003"),
            c("sample_1", "sample_2")
        )
    )
    expect_identical(
        convertGenesToSymbols(mat, release = ensemblRelease) %>%
            rownames(),
        c(
            "ENSMUSG00000000001" = "Gnai3",
            "ENSMUSG00000000003" = "Pbsn"
        )
    )
})

test_that("convertGenesToSymbols : FASTA spike-in support", {
    # Specify organism (to handle FASTA spike-ins (e.g. EGFP)
    vec <- c("EGFP", "ENSMUSG00000000001")
    g2s <- suppressWarnings(
        convertGenesToSymbols(vec, organism = "Mus musculus", release = ensemblRelease)
    )
    expect_identical(g2s, c("EGFP" = "EGFP", "ENSMUSG00000000001" = "Gnai3"))
    expect_warning(
        convertGenesToSymbols(vec, organism = "Mus musculus", release = ensemblRelease),
        "Failed to match all genes to symbols: EGFP"
    )
})

test_that("convertGenesToSymbols : Invalid identifiers", {
    expect_warning(
        convertGenesToSymbols("ENSMUSG00000000000", release = ensemblRelease),
        "Failed to match all genes to symbols: ENSMUSG00000000000"
    )
    expect_error(
        convertGenesToSymbols(
            object = c("ENSMUSG00000000001", "ENSMUSG00000000001"),
            release = ensemblRelease),
        "has_no_duplicates :"
    )
    expect_error(
        convertGenesToSymbols(c("ENSMUSG00000000001", NA)),
        "is_non_missing_nor_empty_character :"
    )
    expect_error(
        convertGenesToSymbols(c("ENSMUSG00000000001", "")),
        "is_non_missing_nor_empty_character :"
    )
})



# convertTranscriptsToGenes ====================================================
test_that("convertTranscriptsToGenes : character", {
    expect_identical(
        convertTranscriptsToGenes(
            c("ENSMUST00000000001", "ENSMUST00000000003"),
            release = ensemblRelease
        ),
        c(
            "ENSMUST00000000001" = "ENSMUSG00000000001",
            "ENSMUST00000000003" = "ENSMUSG00000000003"
        )
    )
    expect_error(
        convertTranscriptsToGenes(
            c("ENSMUST00000000000", "ENSMUST00000000001"),
            release = ensemblRelease
        ),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", NA)),
        "is_non_missing_nor_empty_character :"
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", "")),
        "is_non_missing_nor_empty_character :"
    )
})

test_that("convertTranscriptsToGenes : matrix", {
    mat <- matrix(
        data = seq(1L:8L),
        byrow = TRUE,
        nrow = 4L,
        ncol = 2L,
        dimnames = list(
            c(
                "ENSMUST00000000000",
                "ENSMUST00000000001",
                "ENSMUST00000000003",
                "ENSMUST00000114041"
            ),
            c("sample_1", "sample_2")
        )
    )
    expect_error(
        convertTranscriptsToGenes(mat, release = ensemblRelease),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_identical(
        mat[2L:4L, ] %>%
            convertTranscriptsToGenes() %>%
            rownames(),
        c(
            "ENSMUST00000000001" = "ENSMUSG00000000001",
            "ENSMUST00000000003" = "ENSMUSG00000000003",
            "ENSMUST00000114041" = "ENSMUSG00000000003"
        )
    )
})



# genes ========================================================================
test_that("genes : character", {
    # Loop across the different returns
    x <- mapply(
        FUN = genes,
        return = ensemblReturn,
        MoreArgs = list(x = "Homo sapiens", release = ensemblRelease),
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )
    expect_identical(
        vapply(
            X = x,
            FUN = class,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        ),
        ensemblReturn
    )

    # Dimensions
    n <- 63970L
    dim <- c(n, 11L)
    expect_identical(dim(x[["data.frame"]]), dim)
    expect_identical(dim(x[["DataFrame"]]), dim)
    expect_identical(length(x[["GRanges"]]), n)

    # Gene identifiers
    head <- c("ENSG00000000003", "ENSG00000000005", "ENSG00000000419")
    expect_identical(head(rownames(x[["data.frame"]]), 3L), head)
    expect_identical(head(rownames(x[["DataFrame"]]), 3L), head)
    expect_identical(head(names(x[["GRanges"]]), 3L), head)
    expect_identical(
        rownames(x[["data.frame"]]),
        names(x[["GRanges"]])
    )

    # Metadata columns
    mcols <- list(
        "geneID" = "character",
        "geneName" = "character",
        "geneBiotype" = "factor",
        "description" = "character",
        "geneSeqStart" = "integer",
        "geneSeqEnd" = "integer",
        "seqName" = "factor",
        "seqStrand" = "factor",
        "seqCoordSystem" = "factor",
        "entrezID" = "list",
        "broadClass" = "factor"
    )
    expect_identical(lapply(x[["data.frame"]], class), mcols)
    expect_identical(lapply(x[["DataFrame"]], class), mcols)
    expect_identical(
        lapply(mcols(x[["GRanges"]]), class),
        mcols[c(
            "geneID",
            "geneName",
            "geneBiotype",
            "description",
            "seqCoordSystem",
            "entrezID",
            "broadClass"
        )]
    )
})



# gene2symbol ==================================================================
test_that("gene2symbol : character", {
    x <- gene2symbol("Homo sapiens")
    expect_identical(
        x,
        ensembl("Homo sapiens", format = "gene2symbol")
    )
    expect_identical(
        colnames(x),
        c("geneID", "geneName")
    )
})



# ensembl ======================================================================
test_that("ensembl : Unsupported Ensembl release", {
    expect_error(
        ensembl("Homo sapiens", release = 86L),
        "Ensembl annotations for Homo sapiens : 86 were not found"
    )
})

test_that("ensembl : Unsupported organism", {
    expect_error(
        ensembl("XXX", genomeBuild = "YYY", release = ensemblRelease),
        "Ensembl annotations for XXX : YYY : 87 were not found"
    )
})

test_that("ensembl : Multiple organisms", {
    expect_error(
        ensembl(c("Homo sapiens", "Mus musculus")),
        "is_a_string : "
    )
    expect_error(
        ensembl("Homo sapiens", format = "XXX"),
        paste(
            "'arg' should be one of \"genes\", \"gene2symbol\",",
            "\"transcripts\", \"tx2gene\""
        )
    )
})



# transcripts ==================================================================
test_that("transcripts : character", {
    # Loop across the different returns
    x <- mapply(
        FUN = transcripts,
        return = ensemblReturn,
        MoreArgs = list(x = "Homo sapiens", release = ensemblRelease),
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE
    )
    expect_identical(
        vapply(
            X = x,
            FUN = class,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        ),
        ensemblReturn
    )

    # Dimensions
    n <- 216741L
    dim <- c(n, 14L)
    expect_identical(dim(x[["data.frame"]]), dim)
    expect_identical(dim(x[["DataFrame"]]), dim)
    expect_identical(length(x[["GRanges"]]), n)

    # Transcript identifiers
    head <- c("ENST00000000233", "ENST00000000412", "ENST00000000442")
    expect_identical(head(rownames(x[["data.frame"]]), 3L), head)
    expect_identical(head(rownames(x[["DataFrame"]]), 3L), head)
    expect_identical(head(names(x[["GRanges"]]), 3L), head)
    expect_identical(
        rownames(x[["data.frame"]]),
        names(x[["GRanges"]])
    )

    # Metadata columns
    mcols <- list(
        "txID" = "character",
        "txName" = "character",
        "txBiotype" = "factor",
        "geneID" = "character",
        "geneName" = "character",
        "geneBiotype" = "factor",
        "description" = "character",
        "txSeqStart" = "integer",
        "txSeqEnd" = "integer",
        "txCdsSeqStart" = "integer",
        "txCdsSeqEnd" = "integer",
        "txSupportLevel" = "factor",
        "entrezID" = "list",  # AsIs
        "broadClass" = "factor"
    )
    expect_identical(lapply(x[["data.frame"]], class), mcols)
    mcols[["entrezID"]] <- "AsIs"
    expect_identical(lapply(x[["DataFrame"]], class), mcols)
    mcols[["txSeqStart"]] <- NULL
    mcols[["txSeqEnd"]] <- NULL
    expect_identical(lapply(mcols(x[["GRanges"]]), class), mcols)
})



# tx2gene ======================================================================
test_that("tx2gene : character", {
    x <- tx2gene("Homo sapiens")
    expect_identical(
        x,
        ensembl("Homo sapiens", format = "tx2gene")
    )
})



# GRCh37 =======================================================================
test_that("GRCh37 genome build support", {
    # FIXME Need to use EnsDb instead of GRanges from GTF
    tmpmsg <- "Request Homo_sapiens.GRCh37.75 support update"
    # genes
    expect_error(
        genes("Homo sapiens", genomeBuild = "GRCh37"),
        tmpmsg
    )
    # gene2symbol
    expect_error(
        gene2symbol("Homo sapiens", genomeBuild = "GRCh37"),
        tmpmsg
    )
    # transcripts
    expect_error(
        transcripts("Homo sapiens", genomeBuild = "GRCh37"),
        tmpmsg
    )
    # tx2gene
    expect_error(
        tx2gene("Homo sapiens", genomeBuild = "GRCh37"),
        tmpmsg
    )
})
