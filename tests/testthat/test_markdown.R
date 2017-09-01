context("Markdown Utilities")

test_that("mdHeader", {
    expect_equal(
        mdHeader("Header") %>%
            class,
        "knit_asis")
    expect_equal(
        mdHeader("Header", level = 4L) %>%
            as.character,
        "#### Header\n")
    expect_equal(
        mdHeader("Header", tabset = TRUE) %>%
            as.character,
        "## Header {.tabset}\n")
})



test_that("mdList", {
    groceries <- c("milk", "eggs")
    expect_equal(
        mdList(groceries) %>%
            class,
        "knit_asis")
    expect_equal(
        mdList(groceries) %>%
            as.character,
        c("- milk\n", "- eggs\n"))
    expect_equal(
        mdList(groceries, ordered = TRUE) %>%
            as.character,
        c("1. milk\n", "2. eggs\n"))
})
