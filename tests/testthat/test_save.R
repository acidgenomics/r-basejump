context("Save Utilities")

test_that("assignAndSaveData", {
    expect_identical(
        assignAndSaveData("test", mtcars) %>%
            basename(),
        "test.rda"
    )
    expect_message(
        assignAndSaveData("test", mtcars),
        paste("Saving test to", path_real("."))
    )
    file_delete("test.rda")
})

test_that("saveData", {
    paths <- path(path_real("."), "savetest", c("mtcars.rda", "starwars.rda"))
    names(paths) <- c("mtcars", "starwars")
    expect_identical(
        saveData(mtcars, starwars, dir = "savetest", overwrite = TRUE),
        paths
    )
    expect_warning(
        saveData(mtcars, starwars, dir = "savetest", overwrite = FALSE),
        "Skipping"
    )
    dir_delete("savetest")
    expect_error(
        saveData(XXX),
        "object 'XXX' not found"
    )
    expect_error(
        saveData("mtcars"),
        "is_name : X"
    )
    expect_error(
        saveData(mtcars, dir = NULL),
        "is_a_string : dir"
    )
})
