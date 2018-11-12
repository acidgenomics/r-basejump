context("goalie")

g2s <- Gene2Symbol(
    object = DataFrame(
        geneID = paste0("gene", seq_len(2L)),
        geneName = paste0("symbol", seq_len(2L))
    )
)



# assertFormalGene2Symbol ======================================================
test_that("assertFormalGene2Symbol", {
    genes <- g2s[["geneID"]]
    expect_true(is.character(genes))
    object <- DataFrame(
        "sample1" = c(1L, 2L),
        "sample2" = c(3L, 4L),
        row.names = genes
    )
    expect_null(
        assertFormalGene2Symbol(
            object = object,
            genes = genes,
            gene2symbol = g2s
        )
    )
    expect_error(
        object = assertFormalGene2Symbol(
            object = mtcars,
            genes = genes,
            gene2symbol = g2s
        ),
        regexp = "are_identical :"
    )
})



# assertFormalInterestingGroups ================================================
test_that("assertFormalInterestingGroups", {
    expect_silent(
        assertFormalInterestingGroups(
            object = rse,
            interestingGroups = c("genotype", "treatment")
        )
    )

    # Must exist as columns in sampleData.
    expect_error(
        object = assertFormalInterestingGroups(
            object = rse,
            interestingGroups = "XXX"
        ),
        regexp = "is_subset : The element 'XXX'"
    )
})
