context("Developer Functions")



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
        dots(a, b, c),
        list(
            as.name("a"),
            as.name("b"),
            as.name("c")
        )
    )
    expect_identical(
        dots(a, b, c, character = TRUE),
        c("a", "b", "c")
    )
    expect_error(
        dots(a, a, b, c),
        "has_no_duplicates : dots has a duplicate at position 2"
    )
    expect_error(
        dots(),
        "is_non_empty : dots has length 0"
    )
})



# matchInterestingGroups =======================================================
test_that("matchInterestingGroups", {
    expect_identical(
        matchInterestingGroups(rse_bcb)
    )
    expect_identical(
        matchInterestingGroups(rse_bcb, interestingGroups = NULL),
        "sampleName"
    )
    expect_identical(
        matchInterestingGroups(rse_bcb, interestingGroups = "sampleName"),
        "sampleName"
    )
})



# methodFormals ================================================================
test_that("methodFormals", {
    expect_identical(
        methodFormals(f = "camel", signature = "character"),
        pairlist(
            object = substitute(),
            strict = FALSE
        )
    )
})



# multiassignAsEnvir ===========================================================
test_that("multiassignAsEnvir", {
    x <- multiassignAsEnvir(
        rnaseq_counts, single_cell_counts,
        envirName = "example"
    )
    expect_identical(x, c("rnaseq_counts", "single_cell_counts"))
    expect_identical(x, ls(example))
    expect_error(
        multiassignAsEnvir(rnaseq_counts, envirName = parent.frame()),
        "is_a_string : envirName"
    )
})



# printString ==================================================================
test_that("printString", {
    expect_identical(
        printString(c("hello", "world")),
        "[1] \"hello\" \"world\""
    )
})
