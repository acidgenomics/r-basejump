context("saveData")

test_that("saveData", {
    expect_equal(
        saveData(mtcars, starwars, dir = "data",
                 overwrite = TRUE, quiet = TRUE),
        c(mtcars = file.path(getwd(), "data", "mtcars.rda"),
          starwars = file.path(getwd(), "data", "starwars.rda"))
    )
    expect_message(
        saveData(mtcars, starwars, dir = "data", overwrite = TRUE),
        paste("Saving mtcars.rda, starwars.rda to", file.path(getwd(), "data"))
    )
    expect_equal(
        saveData(mtcars, starwars, dir = "data",
                 overwrite = FALSE, quiet = TRUE),
        NULL
    )
    expect_message(
        saveData(mtcars, starwars, dir = "data", overwrite = FALSE),
        "Skipping mtcars.rda, starwars.rda"
    )
    expect_error(
        saveData(XXX),
        "object 'XXX' not found"
    )
    # Improve this error message
    expect_error(
        saveData("mtcars"),
        "Dot objects cannot contain arguments"
    )
})

test_that("Invalid arguments", {
    expect_error(
        saveData(mtcars, dir = NULL),
        "'dir' must be a string"
    )
})

unlink("data", recursive = TRUE)
