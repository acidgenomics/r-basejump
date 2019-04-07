context("showSlotInfo")

test_that("showSlotInfo", {
    output <- capture.output(
        showSlotInfo(list(
            dir = "~",
            skip = NULL,
            filtered = TRUE
        ))
    )
    expect_identical(
        object = output,
        expected = c(
            "dir: ~",
            "filtered: TRUE"
        )
    )
})
