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



# methodFormals ================================================================
test_that("methodFormals", {
    expect_identical(
        methodFormals(f = "camel", signature = "character"),
        pairlist(
            "object" = substitute(),
            "strict" = FALSE
        )
    )
})



# multiassignAsEnvir ===========================================================
test_that("multiassignAsEnvir", {
    x <- multiassignAsEnvir(
        rnaseqCounts, singleCellCounts,
        envirName = "example"
    )
    expect_identical(x, c("rnaseqCounts", "singleCellCounts"))
    expect_identical(x, ls(example))
    expect_error(
        multiassignAsEnvir(rnaseqCounts, envirName = parent.frame()),
        "is_a_string : envirName"
    )
})
