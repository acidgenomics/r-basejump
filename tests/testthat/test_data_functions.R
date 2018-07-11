context("Data Functions")



# convertGenesToSymbols ========================================================
test_that("convertGenesToSymbols", {
    x <- convertGenesToSymbols(rse_bcb)
    expect_identical(
        head(rownames(x)),
        c("Cox5a", "Comt", "Dazap2", "Rpl13", "Calm1", "Ddt")
    )
})

test_that("convertGenesToSymbols : unmodified return", {
    expect_warning(
        convertGenesToSymbols(rse_dds),
        "Object does not contain gene-to-symbol mappings"
    )
    x <- suppressWarnings(convertGenesToSymbols(rse_dds))
    expect_identical(rownames(x), rownames(rse_dds))
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



# selectSamples ================================================================
test_that("selectSamples : SummarizedExperiment", {
    x <- selectSamples(rse_dds, condition = "A")
    expect_identical(dim(x), c(1000L, 6L))
    expect_identical(colnames(x), paste0("sample", seq(6L)))
})
