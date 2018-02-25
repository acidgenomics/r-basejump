context("Atomic Vector Functions")

# grepString ===================================================================
test_that("grepString", {
    expect_identical(
        grepString("gene"),
        "^gene$|^gene,|\\sgene,|\\sgene$"
    )
})



# sortUnique ===================================================================
test_that("sortUnique", {
    expect_identical(
        sortUnique(c("milk", "eggs", "eggs", NA)),
        c("eggs", "milk", NA)
    )
})



# toStringUnique ===============================================================
test_that("toStringUnique", {
    vec <- c("hello", "world", NA, "hello", "world", NA)
    expect_identical(
        toStringUnique(vec),
        "hello, world"
    )
    expect_error(
        toStringUnique(mtcars),
        "is_atomic"
    )
})
