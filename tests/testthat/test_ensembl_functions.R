context("Ensembl Functions")

# annotable
# ensembl
# genes
#
# gene2symbol
# gene2symbolFromGFF
#
# transcripts
#
# tx2gene
# tx2geneFromGFF

# genes ========================================================================
test_that("genes", {
    nGene <- 63970L
    nMeta <- 11L
    dim <- c(nGene, nMeta)

    # Loop across the different returns
    x <- mapply(
        FUN = ensembl,
        return = ensemblReturn,
        MoreArgs = list(
            organism = organism,
            release = ensemblRelease),
        SIMPLIFY = FALSE,
        USE.NAMES = TRUE)
    expect_identical(
        vapply(
            X = x,
            FUN = class,
            FUN.VALUE = character(1L),
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE),
        ensemblReturn
    )

    # Dimensions
    expect_identical(dim(x[["data.frame"]]), dim)
    expect_identical(dim(x[["DataFrame"]]), dim)
    expect_identical(length(x[["GRanges"]]), nGene)

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
        "ensgene" = "character",
        "symbol" = "character",
        "description" = "character",
        "biotype" = "character",
        "broadClass" = "character",
        "geneSeqStart" = "integer",
        "geneSeqEnd" = "integer",
        "seqName" = "character",
        "seqStrand" = "integer",
        "seqCoordSystem" = "character",
        "entrez" = "list")
    expect_identical(lapply(x[["data.frame"]], class), mcols)
    expect_identical(lapply(x[["DataFrame"]], class), mcols)
    expect_identical(
        lapply(mcols(x[["GRanges"]]), class),
        mcols[c(
            "ensgene",
            "symbol",
            "description",
            "biotype",
            "broadClass",
            "seqCoordSystem",
            "entrez"
        )]
    )
})

test_that("GRCh37 legacy genome build", {
    # genes
    expect_identical(
        annotable(organism, genomeBuild = "GRCh37"),
        grch37
    )
    expect_identical(
        annotable(organism, genomeBuild = "hg19"),
        grch37
    )

    # gene2symbol
    expect_identical(
        annotable(
            organism,
            genomeBuild = "GRCh37",
            format = "gene2symbol"),
        grch37[, c("ensgene", "symbol")]
    )

    # transcripts
    # TODO Add support for this in future update
    expect_error(
        annotable(organism, format = "transcripts", genomeBuild = "GRCh37"),
        "Ensembl annotations for Homo sapiens : GRCh37 were not found"
    )

    # tx2gene
    expect_identical(
        annotable(
            organism,
            genomeBuild = "GRCh37",
            format = "tx2gene"),
        grch37Tx2gene
    )
})

test_that("annotable", {
    # Legacy `annotable()` returns data.frame
    expect_identical(
        annotable(organism, release = ensemblRelease),
        genes(organism, release = ensemblRelease, return = "data.frame")
    )

    # data.frame
    x <- annotable(grch37)
    expect_identical(dim(x), c(63677L, 10L))
    expect_identical(
        lapply(x, class),
        list(
            ensgene = "character",
            symbol = "character",
            description = "character",
            biotype = "character",
            broadClass = "character",
            chr = "character",
            start = "integer",
            end = "integer",
            strand = "integer",
            entrez = "list"
        )
    )

    # Only full gene annotables are supported
    expect_error(
        annotable(grch37[, c("ensgene", "symbol")]),
        "is_subset :"
    )
})

test_that("DataFrame AsIs coercion", {
    # `rowData()` will output `entrez` column as `AsIs` instead of `list`
    # TODO Add support and testing for DataFrame class
    expect_warning(
        annotable(annotable_AsIs),
        paste(
            "Genes without annotations:",
            "ENSMUSG00000101738, ENSMUSG00000104475, ENSMUSG00000109048"
        )
    )
    x <- suppressWarnings(annotable(annotable_AsIs))
    expect_is(x[["entrez"]], "list")
})

test_that("Annotable tibble with duplicate ID rows", {
    # GRCh38 tibble from annotables package
    expect_true(any(duplicated(grch38[["ensgene"]])))
    x <- annotable(grch38)
    expect_true(!any(duplicated(x[["ensgene"]])))
})

test_that("annotable : Unique symbol mode", {
    anno <- annotable(organism, uniqueSymbol = TRUE)
    g2s <- annotable(
        object = "Homo sapiens",
        format = "gene2symbol",
        uniqueSymbol = TRUE)
    # Check for `.1` in `symbol` column
    expect_true(any(grepl("\\.1$", anno[["symbol"]])))
    expect_true(any(grepl("\\.1$", g2s[["symbol"]])))
    # Ensure that symbols aren't duplicated
    expect_true(!any(duplicated(anno[["symbol"]])))
    expect_true(!any(duplicated(g2s[["symbol"]])))
})



# General ======================================================================
test_that("Unsupported Ensembl release", {
    expect_error(
        ensembl(organism, release = 86L),
        "is_greater_than_or_equal_to : "
    )
})

test_that("Unsupported organism", {
    expect_error(
        ensembl("XXX"),
        "Ensembl annotations for XXX were not found in AnnotationHub"
    )
})

test_that("Bad input", {
    expect_error(
        ensembl(c("human", "mouse")),
        "is_a_string : "
    )
    expect_error(
        ensembl(organism, format = "XXX"),
        "is_subset : The element 'XXX' in format is not in"
    )
})



# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols : character", {
    x <- convertGenesToSymbols(
        c("ENSMUSG00000000001", "ENSMUSG00000000003"),
        release = ensemblRelease)
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

test_that("convertGenesToSymbols : Unique symbol mode", {
    g2s <- gene2symbol("Homo sapiens", uniqueSymbol = FALSE)
    expect_true(any(duplicated(g2s[["symbol"]])))
    # Get the duplicate gene identifiers
    dupes <- g2s %>%
        .[which(duplicated(.[["symbol"]])), "ensgene", drop = TRUE]
    x <- convertGenesToSymbols(
        object = dupes,
        gene2symbol = g2s,
        uniqueSymbol = TRUE)
    expect_false(any(duplicated(x)))
})

test_that("convertGenesToSymbols : FASTA spike-in support", {
    # Specify organism (to handle FASTA spike-ins (e.g. EGFP)
    vec <- c("EGFP", "ENSMUSG00000000001")
    g2s <- suppressWarnings(
        convertGenesToSymbols(
            object = vec,
            organism = "Mus musculus", release = ensemblRelease)
    )
    expect_identical(g2s, c("EGFP" = "EGFP", "ENSMUSG00000000001" = "Gnai3"))
    expect_warning(
        convertGenesToSymbols(
            object = vec,
            organism = "Mus musculus", release = ensemblRelease),
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
test_that("Character", {
    expect_identical(
        convertTranscriptsToGenes(
            c("ENSMUST00000000001", "ENSMUST00000000003"),
            release = ensemblRelease),
        c("ENSMUST00000000001" = "ENSMUSG00000000001",
          "ENSMUST00000000003" = "ENSMUSG00000000003")
    )
    expect_error(
        convertTranscriptsToGenes(
            c("ENSMUST00000000000",
              "ENSMUST00000000001"),
            release = ensemblRelease),
        "Unmatched transcripts present. Try using a GFF file instead."
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", NA)),
        "is_non_missing_nor_empty_character"
    )
    expect_error(
        convertTranscriptsToGenes(c("ENSMUSG00000000001", "")),
        "is_non_missing_nor_empty_character"
    )
})

test_that("Matrix", {
    mat <- matrix(
        data = seq(1L:8L),
        byrow = TRUE,
        nrow = 4L,
        ncol = 2L,
        dimnames = list(
            c("ENSMUST00000000000",
              "ENSMUST00000000001",
              "ENSMUST00000000003",
              "ENSMUST00000114041"),
            c("sample1", "sample2")
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
        c("ENSMUST00000000001" = "ENSMUSG00000000001",
          "ENSMUST00000000003" = "ENSMUSG00000000003",
          "ENSMUST00000114041" = "ENSMUSG00000000003")
    )
})



# gene2symbol ==================================================================
test_that("gene2symbol", {
    x <- gene2symbol(organism)
    expect_identical(
        x,
        ensembl(organism, format = "gene2symbol", return = "data.frame")
    )
    expect_identical(
        colnames(x),
        c("ensgene", "symbol")
    )
})



# tx2gene ======================================================================
test_that("tx2gene", {
    x <- tx2gene(organism)
    expect_identical(
        x,
        ensembl(organism, format = "tx2gene", return = "data.frame")
    )
})
