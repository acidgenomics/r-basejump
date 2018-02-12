context("dots")

test_that("Dots", {
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
        "has_no_duplicates"
    )
    expect_error(
        dots(),
        "is_non_empty"
    )
})
