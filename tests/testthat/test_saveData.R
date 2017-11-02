context("saveData")

test_that("saveData", {
    expect_message(
        saveData(mtcars, starwars, dir = "data"),
        paste("Saving mtcars, starwars to", file.path(getwd(), "data"))
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

unlink("data", recursive = TRUE)
