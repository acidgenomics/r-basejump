context("prepareTemplate")

test_that("prepareTemplate", {
    expect_silent(prepareTemplate())
    expect_message(
        prepareTemplate("setup.R", overwrite = TRUE),
        "Overwriting setup.R"
    )
})

unlink("_footer.Rmd")
unlink("_header.Rmd")
unlink("_output.yaml")
unlink("bibliography.bib")
unlink("setup.R")
