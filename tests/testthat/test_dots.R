context("Dots")

expect_identical(
    dots(mtcars, starwars),
    list(sym("mtcars"), sym("starwars"))
)
expect_identical(
    dots(mtcars, starwars, character = TRUE),
    c("mtcars", "starwars")
)
expect_error(
    dots(mtcars, mtcars),
    "has_no_duplicates :"
)
expect_error(
    dots(),
    "is_non_empty :"
)
