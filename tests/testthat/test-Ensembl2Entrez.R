context("Ensembl2Entrez")

formats <- eval(formals(`Ensembl2Entrez,SummarizedExperiment`)[["format"]])

## Note that this method currently calls `rowData()` and uses DataFrame method.
test_that("SummarizedExperiment / DataFrame", {
    for (format in formats) {
        object <- Ensembl2Entrez(object = rse, format = format)
        expect_s4_class(object, "Ensembl2Entrez")
    }
})

test_that("GRanges", {
    for (format in formats) {
        object <- Ensembl2Entrez(object = rowRanges(rse), format = format)
        expect_s4_class(object, "Ensembl2Entrez")
    }
})
