context("Markdown Utilities")

test_that("mdHeader", {
    expect_equal(
        mdHeader("Header") %>%
            class,
        "knit_asis")
    expect_equal(
        mdHeader("Header", level = 4L) %>%
            as.character,
        "#### Header\n")  # nolint
    expect_equal(
        mdHeader("Header", tabset = TRUE) %>%
            as.character,
        "## Header {.tabset}\n")  # nolint
    expect_output(
        mdHeader("Header", asis = TRUE),
        "\\n\\n## Header\\n")  # nolint
    expect_error(
        mdHeader("Header", level = 8L),
        "Markdown supports 1-7 header levels")
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
    expect_output(
        mdList(groceries, asis = TRUE),
        "\\n- milk\\n- eggs\\n")
})
