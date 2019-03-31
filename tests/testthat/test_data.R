# FIXME This context label is too vague.

# nolint start
DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
IRanges <- IRanges::IRanges
# nolint end

data(rse, sce, package = "acidtest", envir = environment())



context("Data")



# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols : SummarizedExperiment", {
    object <- convertGenesToSymbols(rse)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneName"]])
    )
})



# convertSymbolsToGenes ========================================================
test_that("convertSymbolsToGenes : SummarizedExperiment", {
    object <- convertGenesToSymbols(rse)
    object <- convertSymbolsToGenes(object)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneID"]])
    )
})



# counts =======================================================================
test_that("counts", {
    object <- counts(rse)
    expect_is(object, "matrix")
})



# Gene2Symbol ==================================================================
test_that("Gene2Symbol", {
    object <- Gene2Symbol(rse)
    expect_is(object, "DataFrame")
    expect_identical(colnames(object), c("geneID", "geneName"))
    expect_true(hasRownames(object))
})

test_that("Gene2Symbol : No mappings", {
    object <- rse
    mcols(rowRanges(object))[["geneName"]] <- NULL
    expect_error(
        object = Gene2Symbol(object),
        regexp = "geneName"
    )
})



# interestingGroups ============================================================
test_that("interestingGroups : SummarizedExperiment", {
    expect_identical(
        object = interestingGroups(rse),
        expected = "condition"
    )

    # Check object with no metadata
    object <- rse
    metadata(object) <- list()
    expect_identical(interestingGroups(object), NULL)
})

test_that("interestingGroups : Assignment method", {
    object <- rse
    intgroup <- interestingGroups(object)[[1L]]
    interestingGroups(object) <- intgroup
    expect_identical(
        object = interestingGroups(object),
        expected = intgroup
    )
    expect_error(
        object = interestingGroups(object) <- "XXX",
        regexp = "Interesting groups must be columns in `sampleData\\(\\)`."
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

rr <- GRanges(
    seqnames = replicate(n = 4L, expr = "1"),
    ranges = IRanges(
        start = seq(from = 1L, to = 301L, by = 100L),
        end = seq(from = 100L, to = 401L, by = 100L)
    )
)
names(rr) <- genes

cd <- DataFrame(
    genotype = rep(c("wildtype", "knockout"), each = 2L),
    age = rep(c(3L, 6L), times = 2L),
    row.names = samples
)

test_that("makeSummarizedExperiment : RangedSummarizedExperiment", {
    object <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = rr,
        colData = cd
    )
    expect_s4_class(object, "RangedSummarizedExperiment")
    expect_identical(dim(object), c(4L, 4L))
    expect_identical(names(object), genes)
    # Previously we stashed devtoolsSessionInfo and utilsSessionInfo here.
    expect_identical(
        object = lapply(metadata(object), class),
        expected = list(
            date = "Date",
            wd = "character",
            sessionInfo = "session_info"
        )
    )
})

test_that("makeSummarizedExperiment : SummarizedExperiment", {
    # Allow legacy support of rowData pass-in.
    object <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowData = as(rr, "DataFrame"),
        colData = cd
    )
    # Check for SE and not RSE.
    expect_identical(
        object = class(object),
        expected = structure(
            .Data = "SummarizedExperiment",
            package = "SummarizedExperiment"
        )
    )
})

test_that("makeSummarizedExperiment : No row/column annotations", {
    # Ensure this returns clean in minimal mode.
    rse <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = NULL,
        rowData = NULL,
        colData = NULL
    )
    expect_s4_class(rse, "RangedSummarizedExperiment")
    expect_identical(levels(seqnames(rse)), "unknown")
})

test_that("makeSummarizedExperiment : Spike-in support", {
    rownames(mat)[1L:2L] <- c("EGFP", "ERCC")
    object <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = rr[3L:4L],
        colData = cd,
        transgeneNames = "EGFP",
        spikeNames = "ERCC"
    )
    expect_identical(
        object = rownames(object),
        expected = c("EGFP", "ERCC", genes[3L:4L])
    )
    expect_identical(
        object = levels(seqnames(object)),
        expected = c("spike", "transgene", "1")
    )
})

test_that("makeSummarizedExperiment : Strict names", {
    # Don't allow any dashes and other illegal characters in names.
    matBadRows <- mat
    rownames(matBadRows) <- paste0(rownames(mat), "-XXX")
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = matBadRows),
            rowRanges = rr,
            colData = cd
        ),
        regexp = "hasValidDimnames"
    )
    matBadCols <- mat
    colnames(matBadCols) <- paste0(colnames(matBadCols), "-XXX")
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = matBadCols),
            rowRanges = rr,
            colData = cd
        ),
        regexp = "hasValidDimnames"
    )
})

test_that("makeSummarizedExperiment : Duplicate names", {
    matDupeRows <- mat
    rownames(matDupeRows) <- paste0("gene", rep(seq_len(2L), each = 2L))
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = matDupeRows),
            rowRanges = rr,
            colData = cd
        ),
        regexp = "hasValidDimnames"
    )
    matDupeCols <- mat
    colnames(matDupeCols) <- paste0("sample", rep(seq_len(2L), each = 2L))
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = matDupeCols),
            rowRanges = rr,
            colData = cd
        ),
        regexp = "hasValidDimnames"
    )
})

test_that("makeSummarizedExperiment : Column data failure", {
    # Bad pass-in of objects not supporting `dimnames`.
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = "yyy"),
            rowRanges = rr,
            colData = cd
        ),
        regexp = "areIntersectingSets"
    )
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = rr,
            colData = c(xxx = "yyy")
        ),
        regexp = "isAny.*colData"
    )
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = c(xxx = "yyy"),
            colData = cd
        ),
        regexp = "isAny.*rowRanges"
    )
})

test_that("makeSummarizedExperiment : Invalid metadata", {
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = rr,
            colData = cd,
            metadata = Sys.Date()
        ),
        regexp = "isAny.*metadata"
    )
})



# minimalSampleData ============================================================
test_that("minimalSampleData", {
    expect_identical(
        object = minimalSampleData(c("sample 1", "sample 2")),
        expected = DataFrame(
            sampleName = factor(c("sample 1", "sample 2")),
            row.names = factor(c("sample_1", "sample_2"))
        )
    )
})



# sampleData ===================================================================
test_that("sampleData : SummarizedExperiment", {
    object <- rse
    # Check that `sampleName` and `interestingGroups` auto-populate.
    expect_identical(
        object = setdiff(
            x = colnames(sampleData(object)),
            y = colnames(colData(object))
        ),
        expected = c("sampleName", "interestingGroups")
    )
    expect_identical(
        object = sampleData(object)[, colnames(colData(object)), drop = FALSE],
        expected = colData(object)
    )

    # Empty `colData` is supported. Changed in v0.99.
    object <- rse
    colData(object) <- DataFrame(row.names = colnames(object))
    interestingGroups(object) <- NULL
    expect_silent(sampleData(object))
})

test_that("sampleData<- : SummarizedExperiment", {
    object <- rse
    sampleData(object)[["test"]] <- as.factor(seq_len(ncol(object)))
    expect_is(sampleData(object)[["test"]], "factor")
})



# sampleNames ==================================================================
test_that("sampleNames", {
    object <- sampleNames(rse)
    expected <- as.character(sampleData(rse)[["sampleName"]])
    names(expected) <- colnames(rse)
    expect_identical(object, expected)
})



# selectSamples ================================================================
test_that("selectSamples : SummarizedExperiment", {
    object <- selectSamples(rse, condition = "A")
    expect_s4_class(object, class = "SummarizedExperiment")
    expect_identical(
        object = colnames(object),
        expected = paste0("sample0", seq_len(6L))
    )
})

test_that("selectSamples : SingleCellExperiment", {
    object <- selectSamples(sce, sampleID = "sample1")
    expect_identical(
        object = sampleNames(object),
        expected = c(sample1 = "sample1")
    )
    expect_identical(
        object = rownames(sampleData(object)),
        expected = "sample1"
    )
})



# uniteInterestingGroups =======================================================
test_that("uniteInterestingGroups", {
    object <- as(datasets::mtcars, "DataFrame")

    # Check that `interestingGroups` column gets defined as a factor correctly.
    x <- uniteInterestingGroups(
        object = object,
        interestingGroups = c("vs", "am", "gear")
    )
    expect_identical(
        levels(x[["interestingGroups"]]),
        c("0:0:3", "0:1:4", "0:1:5", "1:0:3", "1:0:4", "1:1:4", "1:1:5")
    )

    # Error on missing groups.
    expect_error(
        object = uniteInterestingGroups(
            object = object,
            interestingGroups = c("XXX", "YYY")
        ),
        regexp = "isSubset.*interestingGroups"
    )
})
