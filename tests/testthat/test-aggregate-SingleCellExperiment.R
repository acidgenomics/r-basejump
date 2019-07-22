## This check currently fails on AppVeyor because the file won't load.
skip_on_appveyor()

sce_lanesplit <- readRDS(file.path("cache", "sce_lanesplit.rds"))  # nolint



context("aggregateCols")

test_that("sce_lanesplit", {
    object <- sce_lanesplit
    object <- aggregateCols(object, fun = "sum")
    expect_s4_class(object, "SingleCellExperiment")
    expect_identical(dim(object), c(100L, 6432L))
    sums <- counts(object) %>%
        Matrix::colSums(.) %>%
        as("integer") %>%
        sort(decreasing = TRUE) %>%
        head()
    expect_identical(
        object = sums,
        expected = c(
            CD3H_CCGTAA_CGGTCC_TTCTTG = 439L,
            CD3I_CGCATA_GGTGCT_TCTAGC = 423L,
            CD3I_CGCATA_GGATTG_CATAGA = 403L,
            CD3H_CTAGGT_CTCAAT_TGCGGT = 381L,
            CD3H_TTCTTG_CAACCG_CTATTA = 376L,
            CD3H_GCCGTT_TGGCAG_ATATAC = 340L
        )

    )
})



context("aggregateCellsToSamples")

test_that("sce_lanesplit", {
    object <- sce_lanesplit
    expect_error(
        object = aggregateCellsToSamples(object),
        regexp = "aggregate"
    )
    colData(object)[["aggregate"]] <- NULL
    object <- aggregateCellsToSamples(object)
    sums <- assay(object) %>%
        Matrix::colSums() %>%
        as("integer") %>%
        sort(decreasing = TRUE) %>%
        head()
    expect_identical(
        object = sums,
        expected = c(
            CD3H_3_L001 = 4764L,
            CD3H_3_L002 = 4600L,
            CD3H_3_L003 = 4325L,
            CD3H_3_L004 = 4279L,
            CD3H_1_L001 = 3991L,
            CD3H_1_L002 = 3961L
        )
    )
})

test_that("acidtest::sce", {
    object <- aggregateCellsToSamples(sce)
    sums <- assay(object) %>%
        Matrix::colSums(.) %>%
        as("integer")
    expect_identical(
        object = sums,
        expected = c(
            sample1 = 12622L,
            sample2 = 7011L
        )
    )
})
