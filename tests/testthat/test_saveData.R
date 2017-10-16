context("saveData")

test_that("saveData", {
    expect_message(
        saveData(mtcars, starwars),
        "Saving mtcars, starwars to data"
    )
    expect_error(
        saveData("mtcars"),
        "Dot objects cannot contain arguments"
    )
    expect_error(
        saveData(XXX),
        "object 'XXX' not found"
    )
})

unlink(file.path("data", "mtcars.rda"))
unlink(file.path("data", "starwars.rda"))
