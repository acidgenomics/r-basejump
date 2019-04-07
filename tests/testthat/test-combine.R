context("combine")

test_that("SummarizedExperiment", {
    x <- rse

    # Create a copy of our minimal example.
    y <- x
    colnames(y) <- paste0("sample", seq(from = ncol(y) + 1L, to = ncol(y) * 2L))

    # Combine two SummarizedExperiment objects.
    c <- combine(x, y)
    expect_s4_class(c, "RangedSummarizedExperiment")

    samples <- paste0(
        "sample",
        str_pad(
            string = seq_len(24L),
            width = 2L,
            side = "left",
            pad = "0"
        )
    )
    expect_identical(colnames(c), samples)

    colData <- DataFrame(
        condition = as.factor(rep(rep(c("A", "B"), each = 6L), times = 2L)),
        row.names = samples
    )
    # This will error out due to elementMetadata difference otherwise.
    expect_identical(
        object = as.data.frame(colData(c)),
        expected = as.data.frame(colData)
    )
})

test_that("SingleCellExperiment", {
    x <- sce

    # Here we're faking a distinct replicate, just as an example.
    y <- x
    # Increase the cell ID numbers.
    cells <- colnames(y) %>%
        sub("cell", "", .) %>%
        as.integer() %>%
        `+`(ncol(y)) %>%
        paste0("cell", .)
    colnames(y) <- cells

    # Increase the sample ID numbers.
    sampleID <- y[["sampleID"]]
    sampleID <- gsub("1$", "3", sampleID)
    sampleID <- gsub("2$", "4", sampleID)
    y[["sampleID"]] <- as.factor(sampleID)

    # Combine two SingleCellExperiment objects.
    c <- combine(x, y)
    expect_s4_class(c, "SingleCellExperiment")

    samples <- paste0("sample", seq_len(4L))
    sampleData <- DataFrame(
        sampleName = as.factor(samples),
        interestingGroups = as.factor(samples),
        row.names = samples
    )
    expect_identical(sampleData(c), sampleData)
})
