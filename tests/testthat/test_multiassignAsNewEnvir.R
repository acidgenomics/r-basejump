context("multiassignAsNewEnvir")

test_that("multiassignAsNewEnvir", {
    expect_message(
        multiassignAsNewEnvir(mtcars, starwars, envirName = "test"),
        "Assigning mtcars, starwars as test")
    expect_identical(
        multiassignAsNewEnvir(mtcars, starwars, envirName = "test"),
        c("mtcars", "starwars"))
    expect_error(
        multiassignAsNewEnvir(mtcars, envirName = parent.frame()),
        "is_a_string")
})
