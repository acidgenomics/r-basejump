context("extra | geneSynonyms")

organisms <- eval(formals("geneSynonyms")[["organism"]])

with_parameters_test_that(
    "geneSynonyms", {
        object <- geneSynonyms(organism = organism)
        expect_is(object, "grouped_df")
    },
    organism = organisms
)
