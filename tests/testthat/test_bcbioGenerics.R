context("bcbioGenerics")

test_that("generic definition", {
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

test_that("no methods defined", {
    error <- "unable to find an inherited method"
    expect_error(aggregateReplicates(), error)
    expect_error(bcbio(), error)
    expect_error(
        bcbio() <- "xxx",
        "invalid \\(NULL\\) left side of assignment"
    )
    expect_error(flatFiles(), error)
    expect_error(interestingGroups(), error)
    expect_error(metrics(), error)
    expect_error(plotGene(), error)
    expect_error(sampleDirs(), error)
    expect_error(sampleMetadata(), error)
    expect_error(selectSamples(), error)
})
