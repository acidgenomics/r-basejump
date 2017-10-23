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
        sampleMetadata,
        selectSamples)
    classes <- vapply(generics, class, "character")
    expect_true(all(classes == "nonstandardGenericFunction"))
})

test_that("no methods defined", {
    error <- "unable to find an inherited method"

    # Missing signature
    expect_error(aggregateReplicates(), error)
    expect_error(bcbio(), error)
    expect_error(flatFiles(), error)
    expect_error(interestingGroups(), error)
    expect_error(metrics(), error)
    expect_error(plotGene(), error)
    expect_error(sampleMetadata(), error)
    expect_error(selectSamples(), error)

    # Assignment without method
    lst <- list()
    expect_error(
        bcbio(lst) <- "xxx",
        error
    )
    expect_error(
        interestingGroups(lst) <- "xxx",
        error
    )
})
