context("combine")

test_that("SummarizedExperiment", {
    x <- rse
    colnames(x) <- paste0(
        "sample",
        str_pad(
            string = seq_len(ncol(x)),
            width = 2L,
            pad = "0"
        )
    )

    # Create a copy of our minimal example.
    y <- x
    colnames(y) <- paste0(
        "sample",
        str_pad(
            string = seq(from = ncol(y) + 1L, to = ncol(y) * 2L),
            width = 2L,
            pad = "0"
        )
    )

    # Combine the two objects.
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
    colnames(x) <- paste0(
        "cell",
        str_pad(
            string = seq_len(ncol(x)),
            width = 4L,
            pad = "0"
        )
    )

    # Here we're faking a distinct replicate, just as an example.
    y <- x
    colnames(y) <- paste0(
        "cell",
        str_pad(
            string = seq_len(ncol(y)) + ncol(y),
            width = 4L,
            pad = "0"
        )
    )

    # Increase the sample ID numbers.
    sampleID <- y[["sampleID"]]
    sampleID <- gsub("1$", "3", sampleID)
    sampleID <- gsub("2$", "4", sampleID)
    y[["sampleID"]] <- as.factor(sampleID)

    # Combine the two objects.
    c <- combine(x, y)
    expect_s4_class(c, "SingleCellExperiment")
    expect_identical(
        sort(rownames(sampleData(c))),
        paste0("sample", seq_len(4L))
    )
})
