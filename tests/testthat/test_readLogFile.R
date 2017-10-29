context("readLogFile")

test_that("readLogFile", {
    log <- readLogFile(
        "http://basejump.seq.cloud/bcbio-nextgen.log",
        quiet = TRUE)
    expect_true(is.character(log))
    expect_equal(
        log[[1]],
        paste("[2017-08-15T14:53Z]",
              "compute-a-16-44.o2.rc.hms.harvard.edu:",
              "System YAML configuration:",
              "/n/app/bcbio/dev/galaxy/bcbio_system.yaml")
    )
})
