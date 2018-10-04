context("dplyr method support for DataFrame")

test_that("arrange", {
    expect_identical(
        df %>%
            arrange(desc(sample1)) %>%
            rownames(),
        c(
            "ENSG00000000004",
            "ENSG00000000003",
            "ENSG00000000002",
            "ENSG00000000001"
        )
    )
})

test_that("filter", {
    # filter
    # filter_all
    # filter_at
    # filter_if
})

test_that("left_join", {

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
