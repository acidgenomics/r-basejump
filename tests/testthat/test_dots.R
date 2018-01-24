context("dots")

test_that("dots", {
    expect_identical(
        dots(mtcars, starwars),
        list(rlang::sym("mtcars"),
             rlang::sym("starwars"))
    )
    expect_identical(
        dots(mtcars, starwars, character = TRUE),
        c("mtcars", "starwars")
    )
    expect_error(
        dots(mtcars, mtcars),
        "Duplicate dots: mtcars"
    )
    expect_error(
        dots(mtcars, mtcars, character = TRUE),
        "Duplicate dots: mtcars"
    )
    expect_error(
        dots(),
        "No dots to return"
    )
})
