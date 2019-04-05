context("zerosVsDepth")

test_that("SummarizedExperiment", {
    x <- zerosVsDepth(rse)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = as.data.frame(head(x)[, seq_len(2L)]),
        expected = data.frame(
            dropout = c(0.076, 0.098, 0.092, 0.096, 0.072, 0.100),
            depth = c(19756L, 20700L, 20515L, 20191L, 20604L, 22446L),
            row.names = paste0("sample", "0", seq_len(6L))
        )
    )
})

test_that("SingleCellExperiment", {
    x <- zerosVsDepth(sce)
    expect_s4_class(x, "DataFrame")
    expect_identical(
        object = as.data.frame(head(x)[, seq_len(2L)]),
        expected = data.frame(
            dropout = c(0.574, 0.488, 0.512, 0.476, 0.432, 0.502),
            depth = c(26136L, 44397L, 49120L, 48234L, 87434L, 51542L),
            row.names = paste0("cell", "00", seq_len(6L))
        )
    )
})
