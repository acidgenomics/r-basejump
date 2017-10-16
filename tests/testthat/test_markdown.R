context("markdown")

test_that("mdHeader", {
    md <- mdHeader("Header")
    expect_equal(
        class(md),
        "knit_asis"
    )
    md <- mdHeader("Header", level = 4L) %>%
        as.character()
    expect_equal(
        md,
        "#### Header\n"  # nolint
    )
    md <- mdHeader("Header", tabset = TRUE) %>%
        as.character()
    expect_equal(
         md,
        "## Header {.tabset}\n"  # nolint
    )
    expect_output(
        mdHeader("Header", asis = TRUE),
        "\\n\\n## Header\\n"  # nolint
    )
    expect_error(
        mdHeader("Header", level = 8L),
        "Markdown supports 1-7 header levels"
    )
})



test_that("mdList", {
    groceries <- c("milk", "eggs")
    md <- mdList(groceries)
    expect_equal(
        class(md),
        "knit_asis"
    )
    md <- mdList(groceries) %>%
        as.character()
    expect_equal(
        md,
        c("- milk\n", "- eggs\n")
    )
    md <- mdList(groceries, ordered = TRUE) %>%
        as.character()
    expect_equal(
        md,
        c("1. milk\n", "2. eggs\n")
    )
    expect_output(
        mdList(groceries, asis = TRUE),
        "\\n- milk\\n- eggs\\n"
    )
})
