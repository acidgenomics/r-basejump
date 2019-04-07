context("Gene2Symbol")

format <- methodFormals(
    f = "Gene2Symbol",
    signature = "SummarizedExperiment",
    package = "basejump"
) %>%
    .[["format"]] %>%
    eval()

with_parameters_test_that(
    "Gene2Symbol", {
        object <- Gene2Symbol(rse, format = format)
        expect_s4_class(object, "Gene2Symbol")
        expect_identical(colnames(object), c("geneID", "geneName"))
        expect_true(hasRownames(object))
    },
    format = format
)

rm(format)

test_that("No mappings", {
    object <- rse
    mcols(rowRanges(object))[["geneName"]] <- NULL
    expect_error(
        object = Gene2Symbol(object),
        regexp = "geneName"
    )
})

test_that("summary", {
    x <- Gene2Symbol(rse)
    output <- capture.output(summary(x))
    expect_identical(
        output,
        c(
            "genes: 500",
            "symbols: 500",
            "format: makeUnique",
            "organism: Homo sapiens",
            "genomeBuild: GRCh38",
            "ensemblRelease: 92",
            "id: AH60977",
            "version: 0.1.0",
            "date: 2019-03-27"
        )
    )
})
