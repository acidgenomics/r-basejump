context("assignAndSaveData")

test_that("assignAndSaveData", {
    expect_equal(
        assignAndSaveData("test", mtcars) %>%
            basename(),
        "test.rda"
    )
    expect_message(
        assignAndSaveData("test", mtcars),
        "Saving test to data"
    )
    unlink(file.path("data", "test.rda"))
})
