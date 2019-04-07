context("organism")

with_parameters_test_that(
    "organism", {
        expect_identical(
            object = organism(object),
            expected = "Homo sapiens"
        )
    },
    object = list(
        matrix = mat,
        GRanges = gr,
        SummarizedExperiment = rse
    )
)

test_that("SE metadata stash", {
    org <- "xxx"
    metadata(rse)[["organism"]] <- org
    expect_identical(organism(rse), org)
})

test_that("SE detection via rownames", {
    metadata(rse)[["organism"]] <- NULL
    rownames(rse) <- as.character(rowData(rse)[["geneID"]])
    rowData(rse)[["geneID"]] <- NULL
    expect_identical(
        object = organism(rse),
        expected = "Homo sapiens"
    )
})
