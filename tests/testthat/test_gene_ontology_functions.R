context("Gene Ontology Functions")



# panther ======================================================================
test_that("panther", {
    organisms <- c(
        "Homo sapiens",
        "Mus musculus",
        "Drosophila melanogaster",
        "Caenorhabditis elegans"
    )
    list <- lapply(organisms, function(organism) {
        panther(organism)
    })
})
