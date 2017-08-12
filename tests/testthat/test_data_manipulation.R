context("Data Manipulation Utilities")

test_that("collapse", {
    mpg <- "18.1, 18.7, 21, 21.4, 22.8"

    # data.frame
    expect_equal(
        mtcars %>%
            head %>%
            collapse %>%
            pull("mpg") %>%
            as.character,
        mpg)

    # DataFrame
    expect_equal(
        mtcars %>%
            as("DataFrame") %>%
            head %>%
            collapse %>%
            .[, "mpg"] %>%
            as.character,
        mpg)

    # matrix
    expect_equal(
        mtcars %>%
            as("matrix") %>%
            head %>%
            collapse %>%
            .[, "mpg"] %>%
            as.character,
        mpg)

    # vector
    groceries <- c("milk", "eggs", "eggs", "veggies", NA)
    expect_equal(
        collapse(groceries) %>%
            as.character,
        "eggs, milk, veggies")
    expect_equal(
        collapse(groceries, unique = FALSE, sort = FALSE, keepNA = TRUE) %>%
            as.character,
        "milk, eggs, eggs, veggies, NA")

    # string
    expect_equal(
        collapse("hello"),
        "hello")
})
