context("collapseToString")

mpg <- "18.1, 18.7, 21, 21.4, 22.8"

test_that("character", {
    groceries <- c(NA, "milk", "eggs", "eggs", "veggies")
    expect_identical(
        collapseToString(
            groceries,
            unique = TRUE,
            sort = TRUE),
        "eggs, milk, veggies"
    )
    expect_identical(
        collapseToString(
            groceries,
            unique = TRUE,
            sort = FALSE),
        "milk, eggs, veggies"
    )
    expect_identical(
        collapseToString(
            groceries,
            unique = FALSE,
            sort = FALSE),
        "NA, milk, eggs, eggs, veggies"
    )
    expect_identical(
        collapseToString(
            groceries,
            unique = FALSE,
            sort = TRUE),
        "eggs, eggs, milk, veggies, NA"
    )
})

test_that("integer", {
    expect_identical(
        collapseToString(seq(1L:5L)),
        "1, 2, 3, 4, 5"
    )
})

test_that("numeric", {
    expect_identical(
        collapseToString(c(3.141593, 6.0221409e+23)),
        "3.141593, 6.0221409e+23"
    )
})

test_that("logical", {
    expect_identical(
        collapseToString(c(TRUE, FALSE), sort = TRUE),
        "FALSE, TRUE"
    )
    expect_identical(
        collapseToString(c(NA, NaN), sort = TRUE),
        "NA, NaN"
    )
    expect_identical(
        collapseToString(c(NaN, NA), sort = TRUE),
        "NaN, NA"
    )
})

test_that("data.frame", {
    # data.frame
    expect_identical(
        mtcars %>%
            head() %>%
            collapseToString() %>%
            .[, "mpg"],
        mpg
    )
})

test_that("DataFrame", {
    expect_identical(
        mtcars %>%
            as("DataFrame") %>%
            head() %>%
            collapseToString() %>%
            .[, "mpg"],
        mpg
    )
})

test_that("matrix", {
    expect_identical(
        mtcars %>%
            as("matrix") %>%
            head() %>%
            collapseToString() %>%
            .[1L, "mpg"] %>%
            as.character(),
        mpg
    )
})
