context("PANTHER")



# panther ======================================================================
test_that("panther", {
    organisms <- c(
        "Homo sapiens",
        "Mus musculus",
        "Drosophila melanogaster",
        "Caenorhabditis elegans"
    )
    list <- lapply(organisms, function(organism) {
        invisible(capture.output(
            x <- panther(organism)
        ))
        expect_is(x, "data.frame")
    })
})
