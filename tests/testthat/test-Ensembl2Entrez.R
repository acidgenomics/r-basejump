context("Ensembl2Entrez")

format <- eval(formals(`Ensembl2Entrez,SummarizedExperiment`)[["format"]])

## Note that this method currently calls `rowData()` and uses DataFrame method.
with_parameters_test_that(
    "SummarizedExperiment / DataFrame", {
        object <- Ensembl2Entrez(object = rse, format = format)
        expect_s4_class(object, "Ensembl2Entrez")
    },
    format = format
)

with_parameters_test_that(
    "GRanges", {
        object <- Ensembl2Entrez(object = rowRanges(rse), format = format)
        expect_s4_class(object, "Ensembl2Entrez")
    },
    format = format
)
