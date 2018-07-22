context("Database Functions")



# EggNOG =======================================================================
test_that("EggNOG", {
    x <- eggnog()
    expect_is(x, "list")
    expect_identical(
        names(x),
        c("cogFunctionalCategories", "annotations")
    )
    expect_identical(
        lapply(x, colnames),
        list(
            cogFunctionalCategories = c(
                "letter",
                "description"
            ),
            annotations = c(
                "eggnogID",
                "consensusFunctionalDescription",
                "cogFunctionalCategory"
            )
        )
    )
})



# PANTHER ======================================================================
test_that("PANTHER", {
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
