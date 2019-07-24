context("geneSynonyms")

## Full organism support is covered in extra checks.
skip_if_not(hasInternet())

test_that("geneSynonyms", {
    expect_is(
        object = geneSynonyms(organism = "Homo sapiens"),
        class = "grouped_df"
    )
})
