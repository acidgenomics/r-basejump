context("Object Assignment Utilities")

test_that("assignAndSaveData", {
    expect_message(
        assignAndSaveData("test", mtcars),
        "Saving test to data")
})



test_that("multiassignAsNewEnv", {
    expect_message(
        multiassignAsNewEnv(mtcars, starwars, newEnv = "test"),
        "Assigning mtcars, starwars as test")
    expect_equal(
        multiassignAsNewEnv(mtcars, starwars, newEnv = "test"),
        c("mtcars", "starwars"))
    expect_error(
        multiassignAsNewEnv(mtcars, newEnv = parent.frame()))
})
