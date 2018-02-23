context("Generics")

test_that("S4 Generics", {
    names <- c(
        "aggregateFeatures",
        "aggregateReplicates",
        "camel",
        "collapseToString",
        "convertGenesToSymbols",
        "convertTranscriptsToGenes",
        "detectOrganism",
        "dotted",
        "dynamicPlotlist",
        "foldChangeToLogRatio",
        "fixNA",
        "gene2symbol",
        "gene2symbolFromGFF",
        "geometricMean",
        "kables",
        "logRatioToFoldChange",
        "markdownHeader",
        "markdownList",
        "markdownPlotlist",
        "plotHeatmap",
        "plotQuantileHeatmap",
        "removeNA",
        "snake",
        "stripTranscriptVersions",
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
