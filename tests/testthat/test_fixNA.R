context("fixNA")

test_that("fixNA", {
    # character vector
    expect_identical(
        fixNA(c(1L, "x", "", "NA")),
        c("1", "x", NA, NA))

    # data.frame
    expect_identical(
        data.frame(
            "a" = c("foo", ""),
            "b" = c(NA, "bar"),
            stringsAsFactors = FALSE) %>%
            fixNA(),
        data.frame(
            "a" = c("foo", NA),
            "b" = c(NA, "bar"),
            stringsAsFactors = FALSE)
    )

    # DataFrame
    expect_identical(
        DataFrame(a = c("foo", ""),
                  b = c(NA, "bar")) %>%
            fixNA(),
        DataFrame(a = c("foo", NA),
                  b = c(NA, "bar")))

    # tbl_df
    expect_identical(
        tibble(a = c("foo", ""),
               b = c(NA, "bar")) %>%
            fixNA(),
        tibble(a = c("foo", NA),
               b = c(NA, "bar")))

    # ANY (list)
    expect_identical(
        fixNA(list(a = 1L)),
        list(a = 1L))
})
