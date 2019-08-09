context("filterCells")

object <- sce

test_that("No filtering", {
    ## Expecting an object with the same dimensions by default.
    invisible(capture.output(
        x <- filterCells(object)
    ))
    expect_s4_class(x, "bcbioSingleCell")
    expect_identical(dim(x), dim(object))
})

test_that("Expected cutoff failure", {
    expect_error(
        filterCells(object, minCounts = Inf),
        "No cells passed `minCounts` cutoff"
    )
})

with_parameters_test_that(
    "Parameterized cutoff tests", {
        args[["object"]] <- object
        invisible(capture.output(
            x <- do.call(what = filterCells, args = args)
        ))
        expect_s4_class(x, "bcbioSingleCell")
        expect_is(metadata(x)[["filterParams"]], "list")
        expect_is(metadata(x)[["filterCells"]], "character")
        expect_is(metadata(x)[["filterGenes"]], "character")
        expect_identical(metadata(x)[["subset"]], TRUE)
        expect_identical(dim(x), dim)
    },
    ## Refer to the quality control R Markdown for actual recommended cutoffs.
    ## These are skewed, and designed to work with our minimal dataset.
    args = list(
        list(minCounts = 2000L),
        list(maxCounts = 2500L),
        list(minGenes = 45L),
        list(maxGenes = 49L),
        list(maxMitoRatio = 0.1),
        list(minNovelty = 0.5),
        list(minCellsPerGene = 95L)
    ),
    dim = list(
        c(50L, 35L),
        c(50L, 88L),
        c(50L, 95L),
        c(50L, 81L),
        c(50L, 22L),
        c(50L, 81L),
        c(45L, 100L)
    )
)

test_that("Per sample cutoffs", {
    ## Get the count of sample1 (run1_AGAGGATA).
    ## We're applying no filtering to that sample.
    sampleNames <- sampleNames(object)
    expect_identical(
        sampleNames,
        c(multiplexed_AAAAAAAA = "rep_1")
    )
    invisible(capture.output(
        object <- filterCells(
            object = object,
            minCounts = c(rep_1 = 1L),
            maxCounts = c(rep_1 = Inf),
            minGenes = c(rep_1 = 1L),
            maxGenes = c(rep_1 = Inf),
            maxMitoRatio = c(rep_1 = 1L),
            minNovelty = c(rep_1 = 0L)
        )
    ))
    expect_identical(
        object = metadata(object)[["filterParams"]],
        expected = list(
            nCells = Inf,
            minCounts = c(rep_1 = 1L),
            maxCounts = c(rep_1 = Inf),
            minGenes = c(rep_1 = 1L),
            maxGenes = c(rep_1 = Inf),
            minNovelty = c(rep_1 = 0L),
            maxMitoRatio = c(rep_1 = 1L),
            minCellsPerGene = 1L
        )
    )
})

## Coverage to add:
## - .isFiltered on filtered object
## - filterCells with minCounts argument
## - extract method with filterCells applied
## - No cells passing maxCounts cutoff
## - No cells passing minGenes cutoff
## - No cells passing maxGenes cutoff
## - No cells passing minNovelty cutoff
## - No cells passing maxMitoRatio
## - nCells argument set, less than Inf
## - No cells pass nCells argument
## - No cells passed minCellsPerGene
