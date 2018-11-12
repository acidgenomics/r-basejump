context("goalie")

data(rse, envir = environment())

gene2symbol <- Gene2Symbol(
    object = DataFrame(
        geneID = paste0("gene", seq_len(2L)),
        geneName = paste0("symbol", seq_len(2L))
    )
)



# assertFormalGene2Symbol ======================================================
test_that("assertFormalGene2Symbol", {
    genes <- gene2symbol[["geneID"]]
    expect_true(is.character(genes))
    x <- DataFrame(
        "sample1" = c(1L, 2L),
        "sample2" = c(3L, 4L),
        row.names = genes
    )
    expect_null(
        assertFormalGene2Symbol(
            x = x,
            genes = genes,
            gene2symbol = gene2symbol
        )
    )
    expect_error(
        object = assertFormalGene2Symbol(
            x = datasets::mtcars,
            genes = genes,
            gene2symbol = gene2symbol
        ),
        regexp = "are_identical :"
    )
})



# assertFormalInterestingGroups ================================================
test_that("assertFormalInterestingGroups", {
    expect_silent(
        assertFormalInterestingGroups(
            x = rse,
            interestingGroups = c("genotype", "treatment")
        )
    )

    # Must exist as columns in sampleData.
    expect_error(
        object = assertFormalInterestingGroups(
            x = rse,
            interestingGroups = "XXX"
        ),
        regexp = "is_subset : The element 'XXX'"
    )
})
