context("markdownHeader")

test_that("String", {
    md <- markdownHeader("Header")
    expect_is(md, "knit_asis")
    md <- markdownHeader("Header", level = 4L) %>%
        as.character()
    expect_identical(
        md,
        "#### Header\n"  # nolint
    )
    md <- markdownHeader("Header", tabset = TRUE) %>%
        as.character()
    expect_identical(
         md,
        "## Header {.tabset}\n"  # nolint
    )
    expect_output(
        markdownHeader("Header", asis = TRUE),
        "\\n\\n## Header\\n"  # nolint
    )
    expect_error(
        markdownHeader("Header", level = 8L),
        "is_subset"
    )
})
