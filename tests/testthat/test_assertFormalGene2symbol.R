context("assertFormalGene2symbol")

gene2symbol <- gene2symbol("Homo sapiens", release = 87L)
genes <- head(rownames(gene2symbol), 2L)
x <- data.frame(
    sample1 = c(1L, 2L),
    saple2 = c(3L, 4L),
    row.names = genes,
    stringsAsFactors = FALSE)

test_that("Success", {
    expect_silent(assertFormalGene2symbol(x, genes, gene2symbol))
})

test_that("Failure", {
    expect_error(
        assertFormalGene2symbol(mtcars, genes, gene2symbol),
        paste(
            "is_subset :",
            "The elements 'ENSG00000000003', 'ENSG00000000005'",
            "in genes are not in rownames\\(x\\)."
        )
    )
})
