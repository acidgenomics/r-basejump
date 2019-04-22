Rle <- structure("Rle", package = "S4Vectors")  # nolint



context("relevelRowRanges")

test_that("SummarizedExperiment", {
    rr <- rowRanges(rse)
    expect_s4_class(rr, "GRanges")
    x <- relevelRowRanges(rr)
    expect_s4_class(x, "GRanges")
    if (packageVersion("GenomicRanges") < "1.31") {
        AsIs <- "AsIs"  # nolint
    } else {
        AsIs <- "list"  # nolint
    }
    expect_identical(
        object = lapply(mcols(x), class),
        expected = list(
            geneID = Rle,
            geneName = Rle,
            geneBiotype = Rle,
            broadClass = Rle,
            entrezID = AsIs
        )
    )
})



context("relevelColData")

test_that("SummarizedExperiment", {
    cd <- colData(rse)
    expect_s4_class(cd, "DataFrame")
    x <- relevelColData(cd)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = lapply(x, class),
        expected = list(condition = "factor")
    )
})
