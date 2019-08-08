context("organism")

object <- rse
rownames(object) <- as.character(rowData(rse)[["geneID"]])

## nolint start
matrix <- assay(object)
GRanges <- rowRanges(object)
DataFrame <- as(as.data.frame(GRanges), "DataFrame")
## nolint end

with_parameters_test_that(
    "organism", {
        expect_identical(
            object = organism(object),
            expected = "Homo sapiens"
        )
    },
    object = list(
        matrix = matrix,
        DataFrame = DataFrame,
        GRanges = GRanges,
        SummarizedExperiment = object
    )
)

test_that("SE metadata stash", {
    org <- "xxx"
    metadata(object)[["organism"]] <- org
    expect_identical(organism(object), org)
})

rm(object)
