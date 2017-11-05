context("collapseToString")

mpg <- "18.1, 18.7, 21, 21.4, 22.8"

test_that("character", {
    groceries <- c(NA, "milk", "eggs", "eggs", "veggies")
    expect_equal(
        collapseToString(
            groceries,
            unique = TRUE,
            sort = TRUE),
        "eggs, milk, veggies"
    )
    expect_equal(
        collapseToString(
            groceries,
            unique = TRUE,
            sort = FALSE),
        "milk, eggs, veggies"
    )
    expect_equal(
        collapseToString(
            groceries,
            unique = FALSE,
            sort = FALSE),
        "NA, milk, eggs, eggs, veggies"
    )
    expect_equal(
        collapseToString(
            groceries,
            unique = FALSE,
            sort = TRUE),
        "eggs, eggs, milk, veggies, NA"
    )
})

test_that("integer", {
    expect_equal(
        collapseToString(seq(1:5)),
        "1, 2, 3, 4, 5"
    )
})

test_that("numeric", {
    expect_equal(
        collapseToString(c(3.141593, 6.0221409e+23)),
        "3.141593, 6.0221409e+23"
    )
})

test_that("logical", {
    expect_equal(
        collapseToString(c(TRUE, FALSE), sort = TRUE),
        "FALSE, TRUE"
    )
    expect_equal(
        collapseToString(c(NA, NaN), sort = TRUE),
        "NA, NaN"
    )
    expect_equal(
        collapseToString(c(NaN, NA), sort = TRUE),
        "NaN, NA"
    )
})

test_that("data.frame", {
    # data.frame
    expect_equal(
        mtcars %>%
            head() %>%
            collapseToString() %>%
            .[, "mpg"],
        mpg
    )
})

test_that("DataFrame", {
    expect_equal(
        mtcars %>%
            as("DataFrame") %>%
            head() %>%
            collapseToString() %>%
            .[, "mpg"],
        mpg
    )
})

test_that("matrix", {
    expect_equal(
        mtcars %>%
            as("matrix") %>%
            head() %>%
            collapseToString() %>%
            .[1, "mpg"] %>%
            as.character(),
        mpg
    )
})
