context("saveData")

test_that("saveData", {
    expect_identical(
        saveData(
            mtcars, starwars, dir = "data",
            overwrite = TRUE, quiet = TRUE),
        c(
            mtcars = path_join(c(path_real("."), "data", "mtcars.rda")),
            starwars = path_join(c(path_real("."), "data", "starwars.rda"))
        )
    )
    expect_message(
        saveData(mtcars, starwars, dir = "data", overwrite = TRUE),
        paste(
            "Saving mtcars.rda, starwars.rda to",
            path_join(c(path_real("."), "data"))
        )
    )
    expect_warning(
        saveData(
            mtcars, starwars, dir = "data",
            overwrite = FALSE, quiet = TRUE),
        "No files were saved"
    )
    expect_error(
        saveData(XXX, quiet = TRUE),
        "object 'XXX' not found"
    )
    expect_error(
        saveData("mtcars"),
        "is_name : X"
    )
})

test_that("Invalid arguments", {
    expect_error(
        saveData(mtcars, dir = NULL),
        "is_a_string : dir"
    )
})

dir_delete("data")
