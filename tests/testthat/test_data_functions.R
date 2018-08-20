context("Data Functions")



# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols : SummarizedExperiment", {
    x <- convertGenesToSymbols(rse_bcb)
    expect_identical(
        rownames(x),
        mcols(rowRanges(x))[["geneName"]]
    )

    # Unmodified return
    expect_warning(
        convertGenesToSymbols(rse_dds),
        "Object does not contain gene-to-symbol mappings"
    )
    x <- suppressWarnings(convertGenesToSymbols(rse_dds))
    expect_identical(rownames(x), rownames(rse_dds))
})



# convertSymbolsToGenes ========================================================
test_that("convertSymbolsToGenes : SummarizedExperiment", {
    x <- convertGenesToSymbols(rse_bcb)
    y <- convertSymbolsToGenes(x)
    expect_identical(
        rownames(y),
        mcols(rowRanges(y))[["geneID"]]
    )
})



# counts =======================================================================
test_that("counts", {
    x <- counts(rse_dds)
    expect_is(x, "matrix")
})



# gene2symbol ==================================================================
test_that("gene2symbol", {
    x <- gene2symbol(rse_bcb)
    expect_is(x, "data.frame")
    expect_identical(colnames(x), c("geneID", "geneName"))
    expect_true(tibble::has_rownames(x))
})

test_that("gene2symbol : NULL return", {
    expect_warning(
        gene2symbol(rse_dds),
        "Object does not contain gene-to-symbol mappings"
    )
    expect_identical(
        suppressWarnings(gene2symbol(rse_dds)),
        NULL
    )
})



# interestingGroups ============================================================
test_that("interestingGroups : SummarizedExperiment", {
    expect_identical(
        interestingGroups(rse_bcb),
        "treatment"
    )
    expect_identical(
        interestingGroups(rse_dds),
        NULL
    )
})

test_that("interestingGroups : Assignment method", {
    x <- rse_bcb
    interestingGroups(x) <- "sampleName"
    expect_identical(
        interestingGroups(x),
        "sampleName"
    )
    expect_error(
        interestingGroups(x) <- "XXX",
        "The interesting groups \"XXX\" are not defined"
    )
})



# makeSummarizedExperiment =====================================================
genes <- paste0("gene", seq_len(4L))
samples <- paste0("sample", seq_len(4L))

mat <- matrix(
    data = seq(1L:16L),
    nrow = 4L,
    ncol = 4L,
    byrow = FALSE,
    dimnames = list(genes, samples)
)
dgc <- as(mat, "dgCMatrix")

rr <- GRanges(
    seqnames = replicate(n = 4L, expr = "1"),
    ranges = IRanges(
        start = c(1L, 101L, 201L, 301L),
        end = c(100L, 200L, 300L, 400L)
    )
)
names(rr) <- genes

cd <- data.frame(
    genotype = c(
        "wildtype",
        "wildtype",
        "knockout",
        "knockout"
    ),
    age = c(3L, 6L, 3L, 6L),
    row.names = samples
)

test_that("makeSummarizedExperiment : RangedSummarizedExperiment", {
    rse <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = rr,
        colData = cd
    )
    expect_s4_class(rse, "RangedSummarizedExperiment")
    expect_identical(dim(rse), c(4L, 4L))
    expect_identical(names(rse), genes)
    expect_identical(
        lapply(metadata(rse), class),
        list(
            date = "Date",
            wd = "character",
            utilsSessionInfo = "sessionInfo",
            devtoolsSessionInfo = "session_info"
        )
    )
})

test_that("makeSummarizedExperiment : Super minimal", {
    rse <- suppressWarnings(makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = NULL,
        colData = NULL
    ))
    expect_s4_class(rse, "RangedSummarizedExperiment")
    expect_identical(levels(seqnames(rse)), "unknown")
})

test_that("makeSummarizedExperiment : Spike-in support", {
    rownames(mat)[1L:2L] <- c("EGFP", "ERCC")
    rse <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = rr[3L:4L],
        colData = cd,
        transgeneNames = "EGFP",
        spikeNames = "ERCC"
    )
    expect_identical(
        rownames(rse),
        c("EGFP", "ERCC", genes[3L:4L])
    )
    expect_identical(
        levels(seqnames(rse)),
        c("spike", "transgene", "1")
    )
})

test_that("makeSummarizedExperiment : Strict names", {
    # Don't allow any dashes and other illegal characters in names
    matBadRows <- mat
    rownames(matBadRows) <- paste0(rownames(matBadRows), "-XXX")
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = matBadRows),
            rowRanges = rr,
            colData = cd
        ),
        "are_identical : makeNames\\(rownames\\(assay\\)"
    )
    matBadCols <- mat
    colnames(matBadCols) <- paste0(colnames(matBadCols), "-XXX")
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = matBadCols),
            rowRanges = rr,
            colData = cd
        ),
        "are_identical : makeNames\\(colnames\\(assay\\)"
    )
})

test_that("makeSummarizedExperiment : Duplicate names", {
    matDupeRows <- mat
    rownames(matDupeRows) <- c(
        "gene_1",
        "gene_1",
        "gene_2",
        "gene_2"
    )
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = matDupeRows),
            rowRanges = rr,
            colData = cd
        ),
        paste(
            "has_no_duplicates :",
            "rownames\\(assay\\) has duplicates at positions 2, 4."
        )
    )
    matDupeCols <- mat
    colnames(matDupeCols) <- c(
        "sample_1",
        "sample_1",
        "sample_2",
        "sample_2"
    )
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = matDupeCols),
            rowRanges = rr,
            colData = cd
        ),
        paste(
            "has_no_duplicates :",
            "colnames\\(assay\\) has duplicates at positions 2, 4."
        )
    )
})

test_that("makeSummarizedExperiment : Column data failure", {
    # Bad pass-in of objects not supporting `dimnames()`
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = "yyy"),
            rowRanges = rr,
            colData = cd
        ),
        "has_dimnames : The dimension names of assay are NULL."
    )
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = rr,
            colData = c(xxx = "yyy")
        ),
        "is2 : colData"
    )
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = c(xxx = "yyy"),
            colData = cd
        ),
        "is2 : rowRanges"
    )
})

test_that("makeSummarizedExperiment : Invalid metadata", {
    expect_error(
        makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = rr,
            colData = cd,
            metadata = Sys.Date()
        ),
        "is2 : metadata"
    )
})



# sampleData ===================================================================
test_that("sampleData: Verbose mode", {
    expect_identical(sampleData(rse_bcb), colData(rse_bcb))
})

test_that("sampleData : Assignment method", {
    x <- rse_bcb
    sampleData(x)[["test"]] <- as.factor(seq_len(ncol(x)))
    expect_is(sampleData(x)[["test"]], "factor")
})



# sampleNames ==================================================================
test_that("sampleNames", {
    x <- sampleNames(rse_bcb)
    expect_identical(
        x[seq_len(2L)],
        c(
            control_rep1 = "control_rep1",
            control_rep2 = "control_rep2"
        )
    )

    x <- sampleNames(rse_dds)
    expect_identical(
        x[seq_len(2L)],
        c(
            sample1 = "sample1",
            sample10 = "sample10"
        )
    )
})



# selectSamples ================================================================
test_that("selectSamples : SummarizedExperiment", {
    x <- selectSamples(rse_dds, condition = "A")
    expect_identical(dim(x), c(1000L, 6L))
    expect_identical(colnames(x), paste0("sample", seq(6L)))
})



# uniteInterestingGroups =======================================================
test_that("uniteInterestingGroups : Single interesting group", {
    x <- uniteInterestingGroups(
        object = datasets::mtcars,
        interestingGroups = c("vs", "am", "gear")
    )
    expect_identical(
        levels(x[["interestingGroups"]]),
        c("0:0:3", "0:1:4", "0:1:5", "1:0:3", "1:0:4", "1:1:4", "1:1:5")
    )
})

test_that("uniteInterestingGroups : tidy (tibble) mode", {
    x <- uniteInterestingGroups(
        object = dplyr::starwars,
        interestingGroups = c("hair_color", "skin_color")
    )
    expect_is(x, "tbl_df")
    expect_is(x[["interestingGroups"]], "factor")
    expect_identical(
        x[["interestingGroups"]] %>%
            as.character() %>%
            head(2L),
        c("blond:fair", "NA:gold")
    )
})

test_that("uniteInterestingGroups : Two interesting groups", {
    x <- uniteInterestingGroups(
        object = datasets::mtcars,
        interestingGroups = c("gear", "carb")
    )
    expect_identical(
        head(x[["interestingGroups"]]),
        factor(
            c("4:4", "4:4", "4:1", "3:1", "3:2", "3:1"),
            levels = c(
                "3:1", "3:2", "3:3", "3:4",
                "4:1", "4:2", "4:4",
                "5:2", "5:4", "5:6", "5:8"
            )
        )
    )
})

test_that("uniteInterestingGroups : Missing groups", {
    expect_error(
        uniteInterestingGroups(
            object = datasets::mtcars,
            interestingGroups = c("XXX", "YYY")
        ),
        "is_subset : The elements 'XXX', 'YYY' in interestingGroups"
    )
})
