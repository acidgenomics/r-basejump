context("melt")

with_parameters_test_that(
    "Default", {
        x <- melt(object)
        expect_s4_class(x, "DataFrame")
    },
    object = list(
        matrix = mat,
        Matrix = sparse,
        DataFrame = df,
        SummarizedExperiment = rse,
        SingleCellExperiment = sce
    )
)

## Here we're checking the handling of zero count genes.
test_that("Per row filtering", {
    object <- rse
    assay(object)[seq_len(2L), ] <- 0L
    x <- melt(object, min = 1L, minMethod = "perRow")
    ## Note that this step shouldn't drop all zeros, only all-zero genes.
    expect_true(any(x[["value"]] == 0L))
})

trans <- eval(formals(`melt,SummarizedExperiment`)[["trans"]])
with_parameters_test_that(
    "trans", {
        object <- rse
        object <- melt(
            object = object,
            min = 1L,
            minMethod = "perRow",
            trans = trans
        )
        expect_s4_class(object, "DataFrame")
        object <- decode(round(head(object[["value"]]), digits = 3L))
        expect_identical(object, expected)
    },
    trans = trans,
    expected = list(
        identity = c(2, 17, 5, 0, 14, 8),  # nolint
        log2 = c(1.585, 4.170, 2.585, 0.000, 3.907, 3.170),
        log10 = c(0.477, 1.255, 0.778, 0.000, 1.176, 0.954)
    )
)
