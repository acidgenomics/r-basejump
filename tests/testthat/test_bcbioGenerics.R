context("bcbioGenerics")

test_that("bcbioGenerics", {
    generics <- list(
        aggregateReplicates,
        bcbio,
        `bcbio<-`,
        flatFiles,
        interestingGroups,
        metrics,
        plotGene,
        sampleDirs,
        sampleMetadata,
        selectSamples)
    classes <- vapply(generics, class, "character")
    expect_true(all(classes == "nonstandardGenericFunction"))
})
