context("AllGenerics")

test_that("S4 generics", {
    names <- c(
        "aggregateFeatures",
        "aggregateReplicates",
        "annotable",
        "camel",
        "collapseToString",
        "detectOrganism",
        "dots",
        "dotted",
        "dynamicPlotlist",
        "foldChangeToLogRatio",
        "fixNA",
        "gene2symbol",
        "gene2symbolFromGFF",
        "geometricMean",
        "grepString",
        "kables",
        "logRatioToFoldChange",
        "mdHeader",
        "mdList",
        "mdPlotlist",
        "plotHeatmap",
        "plotQuantileHeatmap",
        "readFileByExtension",
        "readGFF",
        "readYAML",
        "removeNA",
        "snake",
        "sortUnique",
        "stripTranscriptVersions",
        "symbol2gene",
        "toStringUnique",
        "tx2gene",
        "tx2geneFromGFF",
        "upperCamel"
    )
    generics <- lapply(names, get)
    expect_true(all(
        vapply(
            X = generics,
            FUN = isS4,
            FUN.VALUE = logical(1L)
        )
    ))
})
