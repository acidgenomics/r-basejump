skip_on_travis()
skip_on_appveyor()



context("EggNOG")

test_that("EggNOG", {
    object <- EggNOG()
    expect_s4_class(object, "EggNOG")
})



context("HGNC2Ensembl")

test_that("HGNC2Ensembl", {
    object <- HGNC2Ensembl()
    expect_s4_class(object, "HGNC2Ensembl")
})



context("MGI2Ensembl")

test_that("MGI2Ensembl", {
    object <- MGI2Ensembl()
    expect_s4_class(object, "MGI2Ensembl")
})



context("PANTHER")

with_parameters_test_that(
    "PANTHER", {
        invisible(capture.output(
            object <- PANTHER(organism)
        ))
        expect_s4_class(object, "PANTHER")
    },
    organism = names(.pantherMappings)
)



context("geneSynonyms")

# Full organism support is covered in extra checks.

test_that("geneSynonyms", {
    expect_is(
        object = geneSynonyms(organism = "Homo sapiens"),
        class = "grouped_df"
    )
})
