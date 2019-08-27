context("sampleData : SummarizedExperiment")

test_that("Return", {
    object <- rse
    ## Check that `sampleName` and `interestingGroups` auto-populate.
    expect_identical(
        object = setdiff(
            x = colnames(sampleData(object)),
            y = colnames(colData(object))
        ),
        expected = c("sampleName", "interestingGroups")
    )

    ## nolint start
    ##
    ## Bioc 3.10 is inconsistently returning "DFrame" and/or "DataFrame".
    ## https://github.com/Bioconductor/S4Vectors/blob/master/R/DataFrame-class.R#L20
    ##
    ## DataFrame to DFrame class migration:
    ##
    ## Just a direct DataFrame extension with no additional slot for now. Once
    ## all serialized DataFrame instances are replaced with DFrame instances
    ## (which will take several BioC release cycles) we'll be able to move the
    ## DataFrame slots from the DataFrame class definition to the DFrame class
    ## definition. The final goal is to have DataFrame become a virtual class
    ## with no slots that only extends DataTable, and DFrame a concrete
    ## DataFrame and SimpleList subclass that has the same slots as the current
    ## DataFrame class.
    ##
    ## > class(x)
    ## > [1] "DFrame"
    ## > attr(,"package")
    ## > [1] "S4Vectors"
    ##
    ## > class(y)
    ## > [1] "DataFrame"
    ## > attr(,"package")
    ## > [1] "S4Vectors"
    ##
    ## nolint end
    x <- sampleData(object)[, colnames(colData(object)), drop = FALSE]
    y <- colData(object)
    expect_identical(object = x@listData, expected = y@listData)

    ## Empty `colData` is supported.
    object <- rse
    colData(object) <- DataFrame(row.names = colnames(object))
    interestingGroups(object) <- NULL
    expect_silent(sampleData(object))
})

test_that("Empty return", {
    se <- SummarizedExperiment()
    expect_identical(sampleData(se), DataFrame())
})

test_that("Assignment", {
    object <- rse
    sampleData(object)[["test"]] <- as.factor(seq_len(ncol(object)))
    expect_is(sampleData(object)[["test"]], "factor")
})



context("sampleData : SingleCellExperiment")

## Note that this doesn't sort the row names automatically. Here we're doing
## this internally in the check, to make the example "sce" object more resistant
## to code-breaking updates.
test_that("Return", {
    samples <- paste0("sample", seq_len(2L))
    sd <- sampleData(sce)
    expect_identical(sort(rownames(sd)), samples)
    expect_identical(
        sd[samples, ],
        DataFrame(
            sampleName = as.factor(samples),
            interestingGroups = as.factor(samples),
            row.names = samples
        )
    )
})

test_that("Empty return", {
    sce <- SingleCellExperiment()
    expect_identical(sampleData(sce), DataFrame())
})

test_that("No sample info", {
    colData(sce) <- DataFrame(row.names = colnames(sce))
    expect_identical(
        sampleData(sce),
        DataFrame(
            sampleName = factor("unknown"),
            interestingGroups = factor("unknown"),
            row.names = "unknown"
        )
    )
})

test_that("Assignment", {
    sd <- sampleData(sce) %>% .[sort(rownames(.)), , drop = FALSE]
    batch <- as.factor(seq_len(nrow(sd)))
    sd[["batch"]] <- batch
    sampleData(sce) <- sd
    sd <- sampleData(sce) %>% .[sort(rownames(.)), , drop = FALSE]
    samples <- paste0("sample", seq(2L))
    expect_identical(
        object = sd,
        expected = DataFrame(
            sampleName = as.factor(samples),
            batch = batch,
            interestingGroups = as.factor(samples),
            row.names = samples
        )
    )
})
