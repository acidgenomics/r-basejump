context("Markdown Utilities")

test_that("mdHeader", {
    expect_output(
        mdHeader("Header"),
        "\\n\\n\\#\\# Header\\n")
    expect_output(
        mdHeader("Header", level = 4L),
        "\\n\\n\\#\\#\\#\\# Header\\n")
    expect_output(
        mdHeader("Header", tabset = TRUE),
        "\\n\\n\\#\\# Header \\{\\.tabset\\}\\n")
})



test_that("mdList", {
    groceries <- c("milk", "eggs")
    expect_output(
        mdList(groceries),
        "\\n- milk\\n- eggs\\n")
    expect_output(
        mdList(groceries, ordered = TRUE),
        "\\n1. milk\\n2. eggs\\n")
})
