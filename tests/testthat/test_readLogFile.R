context("readLogFile")

test_that("readLogFile", {
    log <- readLogFile(
        file.path("http://basejump.seq.cloud",
                  "bcbio",
                  "bcbio-nextgen.log"),
        quiet = TRUE)
    expect_true(is.character(log))
    expect_equal(
        log[[1L]],
        paste("[2017-08-15T14:53Z]",
              "compute-a-16-44.o2.rc.hms.harvard.edu:",
              "System YAML configuration:",
              "/n/app/bcbio/dev/galaxy/bcbio_system.yaml")
    )
})

test_that("Missing file", {
    expect_warning(
        readLogFile("XXX.log"),
        "XXX.log missing"
    )
    expect_equal(
        suppressWarnings(
            readLogFile("XXX.log")
        ),
        NULL
    )
})
