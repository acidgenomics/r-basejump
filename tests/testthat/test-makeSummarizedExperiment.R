context("makeSummarizedExperiment")

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

test_that("RangedSummarizedExperiment", {
    object <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = rr,
        colData = cd
    )
    expect_s4_class(object, "RangedSummarizedExperiment")
    expect_identical(dim(object), c(4L, 4L))
    expect_identical(names(object), genes)
    ## Previously we stashed devtoolsSessionInfo and utilsSessionInfo here.
    expect_identical(
        object = lapply(metadata(object), class),
        expected = list(
            date = "Date",
            wd = "character",
            sessionInfo = "session_info"
        )
    )
})

test_that("SummarizedExperiment", {
    ## Allow legacy support of rowData pass-in.
    object <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowData = as(rr, "DataFrame"),
        colData = cd
    )
    ## Check for SE and not RSE.
    expect_identical(
        object = class(object),
        expected = structure(
            .Data = "SummarizedExperiment",
            package = "SummarizedExperiment"
        )
    )
})

test_that("No row/column annotations", {
    ## Ensure this returns clean in minimal mode.
    rse <- makeSummarizedExperiment(
        assays = list(counts = mat),
        rowRanges = NULL,
        rowData = NULL,
        colData = NULL
    )
    expect_s4_class(rse, "RangedSummarizedExperiment")
    expect_identical(levels(seqnames(rse)), "unknown")
})

test_that("Spike-in support", {
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

test_that("Strict names", {
    ## Don't allow any dashes and other illegal characters in names.
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

test_that("Duplicate names", {
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

test_that("Column data failure", {
    ## Bad pass-in of objects not supporting `dimnames`.
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

test_that("Invalid metadata", {
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
