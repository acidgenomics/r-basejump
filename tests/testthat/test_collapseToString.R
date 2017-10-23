test_that("collapseToString", {
    mpg <- "18.1, 18.7, 21, 21.4, 22.8"

    # data.frame
    expect_equal(
        mtcars %>%
            head() %>%
            collapseToString() %>%
            .[, "mpg"] %>%
            as.character(),
        mpg
    )

    # DataFrame
    expect_equal(
        mtcars %>%
            as("DataFrame") %>%
            head() %>%
            collapseToString() %>%
            .[, "mpg"] %>%
            as.character(),
        mpg
    )

    # matrix
    expect_equal(
        mtcars %>%
            as("matrix") %>%
            head() %>%
            collapseToString() %>%
            .[, "mpg"] %>%
            as.character(),
        mpg
    )

    # vector
    groceries <- c("milk", "eggs", "eggs", "veggies", NA)
    expect_equal(
        collapseToString(groceries) %>%
            as.character(),
        "eggs, milk, veggies"
    )
    expect_equal(
        collapseToString(
            groceries,
            unique = FALSE,
            sort = FALSE,
            keepNA = TRUE) %>%
            as.character(),
        "milk, eggs, eggs, veggies, NA"
    )

    # vector with only duplicates
    expect_equal(
        collapseToString(c("hello", "hello"), unique = TRUE),
        "hello"
    )

    # string
    expect_equal(
        collapseToString("hello"),
        "hello"
    )
})
