context("sanitizeNA")

with_parameters_test_that(
    "sanitizeNA", {
        expect_identical(
            object = sanitizeNA(object),
            expected = expected
        )
    },
    object = list(
        character = c(1L, "x", "", "NA"),
        data.frame = data.frame(
            a = c("foo", ""),
            b = c(NA, "bar"),
            stringsAsFactors = FALSE
        ),
        DataFrame1 = DataFrame(
            a = c("foo", ""),
            b = c(NA, "bar"),
            row.names = c("c", "d")
        ),
        DataFrame2 = DataFrame(
            a = c("foo", ""),
            b = c(NA, "bar")
        ),
        tbl_df = tibble(
            a = c("foo", ""),
            b = c(NA, "bar")
        )
    ),
    expected = list(
        character = c("1", "x", NA, NA),
        data.frame = data.frame(
            a = c("foo", NA),
            b = c(NA, "bar"),
            stringsAsFactors = FALSE
        ),
        DataFrame1 = DataFrame(
            a = c("foo", NA),
            b = c(NA, "bar"),
            row.names = c("c", "d")
        ),
        DataFrame2 = DataFrame(
            a = c("foo", NA),
            b = c(NA, "bar")
        ),
        tbl_df = tibble(
            a = c("foo", NA),
            b = c(NA, "bar")
        )
    )
)
