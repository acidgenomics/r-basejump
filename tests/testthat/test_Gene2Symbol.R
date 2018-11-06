context("Gene2Symbol")

# TODO Add coverage for `format` arguments.

format <- methodFormals(
    f = "Gene2Symbol",
    signature = "SummarizedExperiment",
    package = "basejump"
) %>%
    .[["format"]] %>%
    eval()

with_parameters_test_that(
    "Gene2Symbol", {
        x <- Gene2Symbol(rse, format = format)
        expect_s4_class(x, "Gene2Symbol")
    },
    format = format
)
