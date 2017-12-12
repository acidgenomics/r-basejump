context("fixNA")

test_that("fixNA", {
    # character vector
    expect_equal(
        fixNA(c(1L, "x", "", "NA")),
        c("1", "x", NA, NA))

    # data.frame
    expect_equal(
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
    expect_equal(
        DataFrame(a = c("foo", ""),
                  b = c(NA, "bar")) %>%
            fixNA(),
        DataFrame(a = c("foo", NA),
                  b = c(NA, "bar")))

    # tbl_df
    expect_equal(
        tibble(a = c("foo", ""),
               b = c(NA, "bar")) %>%
            fixNA(),
        tibble(a = c("foo", NA),
               b = c(NA, "bar")))

    # ANY (list)
    expect_equal(
        fixNA(list(a = 1L)),
        list(a = 1L))
})
