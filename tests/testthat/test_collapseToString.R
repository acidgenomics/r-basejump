context("collapseToString")

mpg <- "18.1, 18.7, 21, 21.4, 22.8"

test_that("character", {
    groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
    expect_identical(
        collapseToString(
            groceries,
            sort = TRUE,
            removeNA = FALSE,
            unique = FALSE),
        "eggs, eggs, milk, veggies, NA, NA"
    )
    expect_identical(
        collapseToString(
            groceries,
            sort = TRUE,
            removeNA = TRUE,
            unique = FALSE),
        "eggs, eggs, milk, veggies"
    )
    expect_identical(
        collapseToString(
            groceries,
            sort = TRUE,
            removeNA = TRUE,
            unique = TRUE),
        "eggs, milk, veggies"
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
    # NA and NaN should stay fixed even when sorting is enabled
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
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[, "mpg", drop = TRUE],
        mpg
    )
})

test_that("DataFrame", {
    expect_identical(
        mtcars %>%
            as("DataFrame") %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[, "mpg"],
        mpg
    )
})

test_that("matrix", {
    expect_identical(
        mtcars %>%
            as("matrix") %>%
            head() %>%
            collapseToString(sort = TRUE, unique = TRUE) %>%
            .[1L, "mpg", drop = TRUE] %>%
            as.character(),
        mpg
    )
})
