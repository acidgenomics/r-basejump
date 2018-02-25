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
        list(sym("mtcars"), sym("starwars"))
    )
    expect_identical(
        dots(mtcars, starwars, character = TRUE),
        c("mtcars", "starwars")
    )
    expect_error(
        dots(mtcars, mtcars),
        "has_no_duplicates :"
    )
    expect_error(
        dots(),
        "is_non_empty :"
    )
})



# multiassignAsNewEnvir ========================================================
test_that("multiassignAsNewEnvir", {
    expect_message(
        multiassignAsNewEnvir(mtcars, starwars, envirName = "test"),
        "Assigning mtcars, starwars as test")
    expect_identical(
        multiassignAsNewEnvir(mtcars, starwars, envirName = "test"),
        c("mtcars", "starwars"))
    expect_error(
        multiassignAsNewEnvir(mtcars, envirName = parent.frame()),
        "is_a_string")
})
