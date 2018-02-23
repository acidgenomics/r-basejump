context("saveData")

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
