context("PANTHER")

skip_if_not(hasInternet())

with_parameters_test_that(
    "organism", {
        invisible(capture.output(
            object <- PANTHER(organism)
        ))
        expect_s4_class(object, "PANTHER")
    },
    organism = names(.pantherMappings)
)

test_that("show", {
    object <- PANTHER("Homo sapiens")
    expect_output(
        object = show(object),
        regexp = "PANTHER"
    )
})
