context("Object Assignment Utilities")

test_that("assignAndSaveData", {
    expect_message(
        assignAndSaveData("test", mtcars),
        "Saving test to data")
})



test_that("multiassignAsNewEnvir", {
    expect_message(
        multiassignAsNewEnvir(mtcars, starwars, envirName = "test"),
        "Assigning mtcars, starwars as test")
    expect_equal(
        multiassignAsNewEnvir(mtcars, starwars, envirName = "test"),
        c("mtcars", "starwars"))
    expect_error(
        multiassignAsNewEnvir(mtcars, envirName = parent.frame()),
        "'envirName' must be a string")
})
