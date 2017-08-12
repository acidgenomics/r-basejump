context("Report Utilities")

test_that("kables", {
    # Check for knit_asis if kables are forced
    expect_equal(
        list(head(starwars), head(mtcars)) %>%
            kables(force = TRUE) %>%
            class,
        "knit_asis")

    # Check for unmodified return in R session
    expect_identical(
        list(head(starwars), head(mtcars)) %>% kables,
        list(head(starwars), head(mtcars)))
})
