context("Developer")



# detectHPC ====================================================================
test_that("detectHPC", {
    expect_identical(detectHPC(), FALSE)

    # LSF
    Sys.setenv("LSF_ENVDIR" = "XXX")
    expect_identical(detectHPC(), "LSF")
    Sys.unsetenv("LSF_ENVDIR")

    # Slurm
    Sys.setenv("SLURM_CONF" = "XXX")
    expect_identical(detectHPC(), "SLURM")
    Sys.unsetenv("SLURM_CONF")
})



# dots =========================================================================
test_that("dots", {
    expect_identical(
        object = dots(a, b, c),
        expected = list(
            as.name("a"),
            as.name("b"),
            as.name("c")
        )
    )
    expect_identical(
        object = dots(a, b, c, character = TRUE),
        expected = c("a", "b", "c")
    )
    expect_error(
        object = dots(a, a, b, c),
        regexp = "has_no_duplicates : dots has a duplicate at position 2"
    )
    expect_error(
        object = dots(),
        regexp = "is_non_empty : dots has length 0"
    )
})



# matchInterestingGroups =======================================================
test_that("matchInterestingGroups", {
    object <- rse_small
    expect_identical(
        object = matchInterestingGroups(object),
        expected = interestingGroups(object)
    )
    expect_identical(
        object = matchInterestingGroups(
            object = rse_small,
            interestingGroups = interestingGroups(object)[[1L]]
        ),
        expected = interestingGroups(object)[[1L]]
    )
})



# methodFormals ================================================================
test_that("methodFormals", {
    expect_identical(
        object = methodFormals(f = "camel", signature = "character"),
        expected = pairlist(
            object = substitute(),
            strict = FALSE
        )
    )
})



# multiassignAsEnvir ===========================================================
test_that("multiassignAsEnvir", {
    object <- multiassignAsEnvir(
        rse_small, sce_small,
        envirName = "example"
    )
    expected <- c("rse_small", "sce_small")
    expect_identical(object, expected)
    expect_identical(
        object = ls(example),
        expected = expected
    )
    expect_error(
        object = multiassignAsEnvir(rse_small, envirName = parent.frame()),
        regexp = "is_a_string : envirName"
    )
})



# printString ==================================================================
test_that("printString", {
    expect_identical(
        object = printString(c("hello", "world")),
        expected = "[1] \"hello\" \"world\""
    )
})



# standardizeCall ==============================================================
aaa <- "AAA"
bbb <- "BBB"
ccc <- "CCC"

test_that("standardizeCall : Standard function", {
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

test_that("standardizeCall : Inside S4 method", {
    setGeneric(
        name = "testing",
        def = function(a, b, ...) {
            standardGeneric("testing")
        }
    )

    # Method with formals identical to the generic.
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

    # Method with formals that differ from generic.
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
