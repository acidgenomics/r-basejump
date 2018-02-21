context("assignAndSaveData")

test_that("assignAndSaveData", {
    expect_identical(
        assignAndSaveData("test", mtcars) %>%
            basename(),
        "test.rda"
    )
    expect_message(
        assignAndSaveData("test", mtcars),
        paste("Saving test to", path_real("."))
    )
})

file_delete("test.rda")
