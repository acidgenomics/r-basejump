context("transmit")

skip_if_not(hasInternet())
skip_on_appveyor()
skip_on_docker()

## Note that only FTP is currently supported.
remoteDir <- paste(
    "ftp://ftp.ncbi.nlm.nih.gov",
    "genomes",
    "Homo_sapiens",
    sep = "/"
)

test_that("NCBI FTP README file", {
    object <- transmit(
        remoteDir = remoteDir,
        pattern = "README",
        compress = FALSE
    )
    expected <- file.path(getwd(), "README")
    names(expected) <- "README"
    expect_identical(object, expected)

    ## Check that function skips on existing.
    expect_message(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "README",
            compress = FALSE
        ),
        regexp = "All files are already downloaded."
    )

    unlink("README")
})

test_that("Rename and compress", {
    object <- transmit(
        remoteDir = remoteDir,
        pattern = "README",
        rename = "readme.txt",
        compress = TRUE
    )
    expected <- file.path(getwd(), "readme.txt.gz")
    names(expected) <- "README"
    expect_identical(object, expected)

    unlink("readme.txt.gz")
})

test_that("Invalid parameters", {
    expect_error(
        object = transmit(
            remoteDir = "http://steinbaugh.com",
            pattern = "README"
        ),
        regexp = "ftp"
    )
    expect_error(
        object = transmit(
            remoteDir = "ftp://ftp.wormbase.org/pub/",
            pattern = "README"
        ),
        regexp = "remoteFiles"
    )
    expect_error(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "XXX"
        ),
        regexp = "match"
    )
    expect_error(
        object = transmit(
            remoteDir = remoteDir,
            pattern = "README",
            rename = c("XXX", "YYY")
        ),
        regexp = "areSameLength"
    )
})
