context("dplyr : DataFrame")

test_that("arrange", {
    expect_identical(
        object = df %>%
            arrange(desc(sample1)) %>%
            rownames(),
        expected = c(
            "ENSG00000000004",
            "ENSG00000000003",
            "ENSG00000000002",
            "ENSG00000000001"
        )
    )
})

test_that("filter", {
    expect_identical(
        object = df %>%
            filter(sample1 > 2L) %>%
            rownames(),
        expected = c("ENSG00000000003", "ENSG00000000004")
    )
    expect_identical(
        object = df %>%
            filter_all(dplyr::all_vars(. < 15L)) %>%
            rownames(),
        expected = c("ENSG00000000001", "ENSG00000000002")
    )
    # filter_at
    # filter_if
})

test_that("left_join", {
    expect_identical(
        object = left_join(
            x = DataFrame(
                cell = paste0("cell", seq_len(4L)),
                sample = paste0("sample", seq_len(2L)),
                row.names = paste0("row", seq_len(4L))
            ),
            y = DataFrame(
                sample = paste0("sample", seq_len(2L)),
                genotype = c("wildtype", "mutant")
            ),
            by = "sample"
        ),
        expected = DataFrame(
            cell = paste0("cell", seq_len(4L)),
            sample = paste0("sample", seq_len(2L)),
            genotype = c("wildtype", "mutant"),
            row.names = paste0("row", seq_len(4L))
        )
    )
})

test_that("mutate", {
    # mutate
    # mutate_all
    # mutate_at
    # mutate_if
})

test_that("rename", {
    # rename
    # rename_all
    # rename_at
    # rename_if
})

test_that("select", {
    # select
    # select_at
    # select_if
})

test_that("transmute", {
    # transmute
    # transmute_at
    # transmute_if
})
