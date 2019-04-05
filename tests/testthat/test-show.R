# FIXME Consider consolidating these S4 tests into a single file.

test <- getOption("acid.test")
options(acid.test = TRUE)



context("show (print)")

test_that("EggNOG", {
    object <- EggNOG()
    expect_output(
        object = show(object),
        regexp = "EggNOG"
    )
})

test_that("PANTHER", {
    object <- PANTHER("Homo sapiens")
    expect_output(
        object = show(object),
        regexp = "PANTHER"
    )

})



options(acid.test = test)
