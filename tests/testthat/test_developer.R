context("Developer")

data(rse, sce, package = "acidtest", envir = environment())



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
        regexp = "hasNoDuplicates"
    )
    expect_error(
        object = dots(),
        regexp = "hasLength"
    )
})



# matchInterestingGroups =======================================================
test_that("matchInterestingGroups", {
    object <- rse
    expect_identical(
        object = matchInterestingGroups(object),
        expected = interestingGroups(object)
    )
    expect_identical(
        object = matchInterestingGroups(
            object = object,
            interestingGroups = interestingGroups(object)[[1L]]
        ),
        expected = interestingGroups(object)[[1L]]
    )
})




# methodFormals ================================================================
# This will also cover `methodFunction`.
test_that("methodFormals", {
    library(SummarizedExperiment)
    expect_identical(
        object = methodFormals(
            f = "colData",
            signature = "SummarizedExperiment",
            package = "SummarizedExperiment"
        ),
        expected = pairlist(
            "x" = substitute(),
            "..." = substitute()
        )
    )
})



# multiassignAsEnvir ===========================================================
test_that("multiassignAsEnvir", {
    object <- multiassignAsEnvir(rse, sce, envirName = "example")
    expected <- c("rse", "sce")
    expect_identical(object, expected)
    expect_identical(
        object = ls(example),
        expected = expected
    )
    expect_error(
        object = multiassignAsEnvir(rse, envirName = parent.frame()),
        regexp = "isString.*envirName"
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
