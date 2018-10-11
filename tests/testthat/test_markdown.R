context("R Markdown")

load("plotlist.rda")



# markdownHeader ===============================================================
test_that("markdownHeader", {
    object <- markdownHeader("Header")
    expect_is(object, "knit_asis")
    expect_identical(
        object = "Header" %>%
            markdownHeader(level = 4L) %>%
            as.character(),
        expected = "#### Header\n"  # nolint
    )
    expect_identical(
        object = "Header" %>%
            markdownHeader(tabset = TRUE) %>%
            as.character(),
        expected = "## Header {.tabset}\n"  # nolint
    )
    expect_output(
        object = markdownHeader("Header", asis = TRUE),
        regexp = "\\n\\n## Header\\n"  # nolint
    )
    expect_error(
        object = markdownHeader("Header", level = 8L),
        regexp = "is_subset : The element '8'"
    )
})



# markdownLink =================================================================
test_that("markdownLink", {
    expect_identical(
        capture.output(markdownLink(
            text = "R",
            url = "https://www.r-project.org",
            title = "The R Project for Statistical Computing"
        )),
        paste(
            "[R](https://www.r-project.org):",
            "The R Project for Statistical Computing"
        )
    )
})



# markdownList =================================================================
test_that("markdownList", {
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



# markdownPlotlist =============================================================
test_that("markdownPlotlist", {
    output <- capture.output(
        markdownPlotlist(plotlist)
    )
    expect_identical(
        output,
        c(
            "",
            "",
            "## continuous",
            "",
            "",
            "",
            "## discrete",
            ""
        )
    )
})



# markdownTables ===============================================================
test_that("markdownTables : list", {
    # Simulate a knit session.
    object <- markdownTables(
        list = list(head(ggplot2::mpg), head(datasets::mtcars)),
        captions = c("mpg", "mtcars"),
        force = TRUE
    )
    expect_is(object, "knit_asis")

    # Check for unmodified return in R session.
    object <- markdownTables(
        list = list(head(ggplot2::mpg), head(datasets::mtcars)),
        captions = c("mpg", "mtcars"),
        force = FALSE
    )
    expect_is(object, "list")
})



# prepareTemplate ==============================================================
# This code is covered in bcbioRNASeq and bcbioSingleCell.
test_that("prepareTemplate", {
    expect_error(prepareTemplate(package = "XXX"))
    expect_error(prepareTemplate(package = "basejump"))
})
