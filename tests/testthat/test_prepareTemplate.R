context("prepareTemplate")

test_that("prepareTemplate", {
    files <- c(
        "_footer.Rmd",
        "_header.Rmd",
        "_output.yaml",
        "bibliography.bib",
        "setup.R")
    expect_silent(prepareTemplate())
    expect_message(
        prepareTemplate("setup.R", overwrite = TRUE),
        "Overwriting setup.R"
    )
    unlink(files)
})
