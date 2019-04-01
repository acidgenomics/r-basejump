context("Extra : Gene synonyms")

organisms <- .geneSynonymsOrganisms

with_parameters_test_that(
    "geneSynonyms", {
        object <- geneSynonyms(organism = organism)
        expect_is(object, "grouped_df")
        print(organism)
        print(nrow(object))
    },
    organism = organisms,
    # Note that these counts change over time.
    nrow = c(
        homo_sapiens = 19439L,
        mus_musculus = 21330L,
        drosophila_melanogaster = 20252L
    )
)
