context("standardizeCall")

aaa <- "AAA"
bbb <- "BBB"
ccc <- "CCC"

test_that("Standard function", {
    testing <- function(a, b) {
        standardizeCall()
    }
    object <- testing(aaa, bbb)
    expect_is(object, "call")
    expect_identical(
        object = object,
        expected = call(
            name = "testing",
            a = as.symbol("aaa"),
            b = as.symbol("bbb")
        )
    )
})

test_that("Inside S4 method", {
    setGeneric(
        name = "testing",
        def = function(a, b, ...) {
            standardGeneric("testing")
        }
    )

    ## Method with formals identical to the generic.
    setMethod(
        f = "testing",
        signature = signature("character"),
        definition = function(a, b, ...) {
            standardizeCall()
        }
    )
    object <- testing(aaa, bbb)
    expect_is(object, "call")
    expect_identical(
        object = object,
        expected = call(
            name = "testing",
            a = as.symbol("aaa"),
            b = as.symbol("bbb")
        )
    )

    ## Method with formals that differ from generic.
    setMethod(
        f = "testing",
        signature = signature("character"),
        definition = function(a, b, c) {
            standardizeCall()
        }
    )
    object <- testing(aaa, bbb, ccc)
    expect_is(object, "call")
    expect_identical(
        object = object,
        expected = call(
            name = "testing",
            a = as.symbol("aaa"),
            b = as.symbol("bbb"),
            c = as.symbol("ccc")
        )
    )
})
