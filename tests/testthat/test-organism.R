context("organism")

with_parameters_test_that(
    "organism", {
        expect_identical(
            object = organism(object),
            expected = "Homo sapiens"
        )
    },
    object = list(
        matrix = mat,
        SummarizedExperiment = rse
    )
)
