context("removeNA")

test_that("data.frame", {
    # data.frame
    expect_equal(
        data.frame(a = c("A", NA, "C"),
                   b = c(NA, NA, NA),
                   c = c("B", NA, "D")) %>%
            removeNA(),
        data.frame(a = c("A", "C"),
                   c = c("B", "D"),
                   row.names = c(1L, 3L))
    )
})

# Support for vectors (using `stats::na.omit()`)
test_that("vector", {
    expect_equal(
        removeNA(c("hello", "world", NA)) %>%
            as.character(),
        c("hello", "world")
    )
    expect_equal(
        removeNA(c(1L, 2L, NA)) %>%
            as.integer(),
        c(1L, 2L)
    )
})
