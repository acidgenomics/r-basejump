context("markdownList")

test_that("Character", {
    groceries <- c("milk", "eggs")
    md <- markdownList(groceries)
    expect_identical(
        class(md),
        "knit_asis"
    )
    md <- markdownList(groceries) %>%
        as.character()
    expect_identical(
        md,
        c("- milk\n", "- eggs\n")
    )
    md <- markdownList(groceries, ordered = TRUE) %>%
        as.character()
    expect_identical(
        md,
        c("1. milk\n", "2. eggs\n")
    )
    expect_output(
        markdownList(groceries, asis = TRUE),
        "\\n- milk\\n- eggs\\n"
    )
})
