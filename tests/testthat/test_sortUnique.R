context("sortUnique")

test_that("sortUnique", {
    expect_equal(
        sortUnique(c("milk", "eggs", "eggs", NA)),
        c("eggs", "milk")
    )
})
