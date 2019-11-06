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
    ## Check for removal of our all-zero gene.
    expect_identical(
        object = setdiff(x = rownames(object), y = unique(x[["rowname"]])),
        expected = head(rownames(object), n = 2L)
    )
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
        identity = c(14, 19, 19, 107, 128),  # nolint
        log2 = c(3.907, 4.322, 4.322, 6.755, 7.011, 2.322),
        log10 = c(1.176, 1.301, 1.301, 2.033, 2.111, 0.699)
    )
)
