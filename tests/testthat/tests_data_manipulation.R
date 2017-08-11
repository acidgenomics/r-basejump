context("Data Manipulation Utilities")

test_that("collapse", {
    # data.frame
    expect_equal(
        mtcars %>%
            head %>%
            collapse %>%
            pull("mpg") %>%
            as.character,
        "18.1, 18.7, 21, 21.4, 22.8")

    # vector
    groceries <- c("milk", "eggs", "eggs", "veggies", NA)
    expect_equal(
        collapse(groceries) %>%
            as.character,
        "eggs, milk, veggies")
    expect_equal(
        collapse(groceries, unique = FALSE, sort = FALSE) %>%
            as.character,
        "milk, eggs, eggs, veggies")
})
