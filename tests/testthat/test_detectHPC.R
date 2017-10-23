context("detectHPC")

test_that("detectHPC", {
    expect_equal(detectHPC(), FALSE)

    # LSF
    Sys.setenv("LSF_ENVDIR" = "XXX")
    expect_equal(detectHPC(), "LSF")
    Sys.unsetenv("LSF_ENVDIR")

    # Slurm
    Sys.setenv("SLURM_CONF" = "XXX")
    expect_equal(detectHPC(), "SLURM")
    Sys.unsetenv("SLURM_CONF")
})
