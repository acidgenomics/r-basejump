context("transmit")

test_that("Standard", {
    readme <- transmit(
        remoteDir = ensemblURL,
        pattern = "README",
        compress = FALSE)
    expected <- path_join(c(path_real("."), "README"))
    names(expected) <- "README"
    expect_identical(readme, expected)
    file_delete("README")
})

test_that("Rename and compress", {
    readme <- transmit(
        remoteDir = ensemblURL,
        pattern = "README",
        rename = "ensembl_readme.txt",
        compress = TRUE)
    expected <- path_join(c(path_real("."), "ensembl_readme.txt.gz"))
    names(expected) <- "README"
    expect_identical(readme, expected)
    file_delete("ensembl_readme.txt.gz")
})

test_that("Invalid parameters", {
    expect_error(
        transmit("http://steinbaugh.com", pattern = "README"),
        "is_matching_regex : remoteDir"
    )
    expect_error(
        transmit("ftp://ftp.wormbase.org/pub/", pattern = "README"),
        "is_non_empty : remoteFileList"
    )
    expect_error(
        transmit(ensemblURL, pattern = "XXX"),
        "is_non_empty : match"
    )
    expect_error(
        transmit(ensemblURL, pattern = "README", rename = c("XXX", "YYY")),
        "are_same_length : match has length 1 but rename has length 2."
    )
})
