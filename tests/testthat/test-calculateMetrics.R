context("calculateMetrics")

with_parameters_test_that(
    "calculateMetrics", {
        object <- calculateMetrics(object, prefilter = TRUE)
        expect_s4_class(object, "DataFrame")
        ## Check for run-length encoding.
        expect_true(
            all(bapply(X = object, FUN = function(x) { is(x, "Rle") }))
        )
        ## Check that expected values match.
        ## Parameterized unit test is changing `nCount` to NA.
        x <- lapply(
            X = object[1L, c("nUMI", "nGene", "nMito"), drop = TRUE],
            FUN = decode
        )
        y <- list(
            nUMI = 41054L,
            nGene = 235L,
            nMito = NA_integer_
        )
        expect_identical(x, y)
    },
    object = list(
        Matrix = counts(sce)
        ## DelayedArray = DelayedArray(counts(sce)),
        ## SingleCellExperiment = sce
    )
)
