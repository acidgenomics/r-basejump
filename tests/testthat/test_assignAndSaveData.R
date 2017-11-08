context("assignAndSaveData")

test_that("assignAndSaveData", {
    expect_equal(
        assignAndSaveData("test", mtcars) %>%
            basename(),
        "test.rda"
    )
    expect_message(
        assignAndSaveData("test", mtcars),
        paste("Saving test to", getwd())
    )
})

unlink("test.rda")
