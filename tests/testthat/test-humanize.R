context("humanize")

test_that("SummarizedExperiment", {
    samples <- letters[seq_along(colnames(rse))]
    names(samples) <- colnames(rse)
    sampleNames(rse) <- samples
    x <- humanize(rse)
    expect_identical(
        object = lapply(dimnames(x), head),
        expected = list(
            c("TSPAN6", "TNMD", "DPM1", "SCYL3", "C1orf112", "FGR"),
            c("a", "b", "c", "d", "e", "f")
        )
    )
})

test_that("SingleCellExperiment", {
    x <- humanize(sce)
    expect_identical(
        object = lapply(dimnames(x), head),
        expected = list(
            c("TSPAN6", "TNMD", "DPM1", "SCYL3", "C1orf112", "FGR"),
            paste0("cell00", seq_len(6L))
        )
    )
})
