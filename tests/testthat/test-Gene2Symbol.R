context("Gene2Symbol")

test_that("S4 generator", {
    object <- Gene2Symbol(rse)
    expect_is(object, "DataFrame")
    expect_identical(colnames(object), c("geneID", "geneName"))
    expect_true(hasRownames(object))
})

test_that("No mappings", {
    object <- rse
    mcols(rowRanges(object))[["geneName"]] <- NULL
    expect_error(
        object = Gene2Symbol(object),
        regexp = "geneName"
    )
})
