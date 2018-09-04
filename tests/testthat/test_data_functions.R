context("Data Functions")



# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols : SummarizedExperiment", {
    object <- convertGenesToSymbols(rse_small)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneName"]])
    )
})



# convertSymbolsToGenes ========================================================
test_that("convertSymbolsToGenes : SummarizedExperiment", {
    object <- convertGenesToSymbols(rse_small)
    object <- convertSymbolsToGenes(object)
    expect_identical(
        object = rownames(object),
        expected = as.character(mcols(rowRanges(object))[["geneID"]])
    )
})



# counts =======================================================================
test_that("counts", {
    object <- counts(rse_small)
    expect_is(object, "matrix")
})



# gene2symbol ==================================================================
test_that("gene2symbol", {
    object <- gene2symbol(rse_small)
    expect_is(object, "DataFrame")
    expect_identical(colnames(object), c("geneID", "geneName"))
    expect_true(hasRownames(object))
})

test_that("gene2symbol : No mappings", {
    object <- rse_small
    rowData(object) <- NULL
    expect_error(
        object = gene2symbol(object),
        regexp = "Object does not contain gene-to-symbol mappings"
    )
})



# interestingGroups ============================================================
test_that("interestingGroups : SummarizedExperiment", {
    expect_identical(
        object = interestingGroups(rse_small),
        expected = c("genotype", "treatment")
    )

    # Check object with no metadata
    object <- rse_small
    metadata(object) <- list()
    expect_identical(interestingGroups(object), NULL)
})

test_that("interestingGroups : Assignment method", {
    object <- rse_small
    interestingGroups(object) <- "sampleName"
    expect_identical(
        object = interestingGroups(object),
        expected = "sampleName"
    )
    expect_error(
        object = interestingGroups(object) <- "XXX",
        regexp = "is_subset : The element 'XXX' in interestingGroups"
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
    expect_identical(
        object = lapply(metadata(object), class),
        expected = list(
            date = "Date",
            wd = "character",
            utilsSessionInfo = "sessionInfo",
            devtoolsSessionInfo = "session_info"
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
        regexp = "are_identical : makeNames\\(rownames\\(assay\\)"
    )
    matBadCols <- mat
    colnames(matBadCols) <- paste0(colnames(matBadCols), "-XXX")
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = matBadCols),
            rowRanges = rr,
            colData = cd
        ),
        regexp = "are_identical : makeNames\\(colnames\\(assay\\)"
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
        regexp = paste(
            "has_no_duplicates :",
            "rownames\\(assay\\) has duplicates at positions 2, 4."
        )
    )
    matDupeCols <- mat
    colnames(matDupeCols) <- paste0("sample", rep(seq_len(2L), each = 2L))
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = matDupeCols),
            rowRanges = rr,
            colData = cd
        ),
        regexp = paste(
            "has_no_duplicates :",
            "colnames\\(assay\\) has duplicates at positions 2, 4."
        )
    )
})

test_that("makeSummarizedExperiment : Column data failure", {
    # Bad pass-in of objects not supporting `dimnames()`.
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = "yyy"),
            rowRanges = rr,
            colData = cd
        ),
        regexp = "has_dimnames : The dimension names of assay are NULL."
    )
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = rr,
            colData = c(xxx = "yyy")
        ),
        regexp = "is2 : colData"
    )
    expect_error(
        object = makeSummarizedExperiment(
            assays = list(counts = mat),
            rowRanges = c(xxx = "yyy"),
            colData = cd
        ),
        regexp = "is2 : rowRanges"
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
        regexp = "is2 : metadata"
    )
})



# sampleData ===================================================================
test_that("sampleData : SummarizedExperiment", {
    expect_identical(
        object = sampleData(rse_small),
        expected = colData(rse_small)
    )

    # Requiring `sampleName` column to be defined.
    object <- rse_small
    colData(object) <- DataFrame(row.names = colnames(object))
    expect_error(
        object = sampleData(object),
        regexp = "sampleName"
    )
})

test_that("sampleData<- : SummarizedExperiment", {
    object <- rse_small
    sampleData(object)[["test"]] <- as.factor(seq_len(ncol(object)))
    expect_is(sampleData(object)[["test"]], "factor")
})



# sampleNames ==================================================================
test_that("sampleNames", {
    object <- sampleNames(rse_small)
    expected <- as.character(colData(rse_small)[["sampleName"]])
    names(expected) <- colnames(rse_small)
    expect_identical(object, expected)

    # Require `sampleName` column to be defined.
    object <- rse_small
    colData(object) <- DataFrame(row.names = colnames(object))
    expect_error(
        object = sampleNames(object),
        regexp = "sampleName"
    )
})



# selectSamples ================================================================
test_that("selectSamples : SummarizedExperiment", {
    object <- selectSamples(rse_small, genotype = "wildtype")
    expect_identical(
        object = dim(object),
        expected = c(100L, 2L)
    )
    expect_identical(
        object = colnames(object),
        expected = c("sample1", "sample3")
    )
})



# uniteInterestingGroups =======================================================
test_that("uniteInterestingGroups : Single interesting group", {
    object <- uniteInterestingGroups(
        object = datasets::mtcars,
        interestingGroups = c("vs", "am", "gear")
    )
    expect_identical(
        levels(object[["interestingGroups"]]),
        c("0:0:3", "0:1:4", "0:1:5", "1:0:3", "1:0:4", "1:1:4", "1:1:5")
    )
})

test_that("uniteInterestingGroups : Two interesting groups", {
    object <- uniteInterestingGroups(
        object = datasets::mtcars,
        interestingGroups = c("gear", "carb")
    )
    expect_identical(
        head(object[["interestingGroups"]]),
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

test_that("uniteInterestingGroups : tidy (tibble) mode", {
    object <- uniteInterestingGroups(
        object = dplyr::starwars,
        interestingGroups = c("hair_color", "skin_color")
    )
    expect_is(object, "tbl_df")
    expect_is(object[["interestingGroups"]], "factor")
    expect_identical(
        object[["interestingGroups"]] %>%
            as.character() %>%
            head(2L),
        c("blond:fair", "NA:gold")
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
