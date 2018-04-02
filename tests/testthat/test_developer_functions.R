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
        dots(mtcars, starwars),
        list(as.name("mtcars"), as.name("starwars"))
    )
    expect_identical(
        dots(mtcars, starwars, character = TRUE),
        c("mtcars", "starwars")
    )
    expect_error(
        dots(mtcars, mtcars),
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



# multiassignAsNewEnvir ========================================================
test_that("multiassignAsNewEnvir", {
    x <- multiassignAsNewEnvir(mtcars, starwars, envirName = "test")
    expect_identical(x, c("mtcars", "starwars"))
    expect_identical(x, ls(test))
    expect_error(
        multiassignAsNewEnvir(mtcars, envirName = parent.frame()),
        "is_a_string : envirName"
    )
})
