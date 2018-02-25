context("Save Utilities")

# assignAndSaveData ============================================================
test_that("assignAndSaveData", {
    expect_identical(
        assignAndSaveData("test", mtcars) %>%
            basename(),
        "test.rda"
    )
    expect_message(
        assignAndSaveData("test", mtcars),
        paste("Saving test to", path_real("."))
    )
    file_delete("test.rda")
})



# saveData =====================================================================
test_that("saveData", {
    paths <- path(path_real("."), "savetest", c("mtcars.rda", "starwars.rda"))
    names(paths) <- c("mtcars", "starwars")
    expect_identical(
        saveData(mtcars, starwars, dir = "savetest", overwrite = TRUE),
        paths
    )
    expect_warning(
        saveData(mtcars, starwars, dir = "savetest", overwrite = FALSE),
        "Skipping"
    )
    dir_delete("savetest")
    expect_error(
        saveData(XXX),
        "object 'XXX' not found"
    )
    expect_error(
        saveData("mtcars"),
        "is_name : X"
    )
    expect_error(
        saveData(mtcars, dir = NULL),
        "is_a_string : dir"
    )
})



# transmit =====================================================================
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



# writeCounts ==================================================================
test_that("writeCounts", {
    df <- as.data.frame(mtcars)
    dgc <- as(mat, "dgCMatrix")
    expect_message(
        writeCounts(df, dgc, mat, dir = "testcounts"),
        "Writing df, dgc, mat"
    )
    expect_identical(
        dir("testcounts"),
        c(
            "df.csv.gz",
            "dgc.mtx.colnames",
            "dgc.mtx.gz",
            "dgc.mtx.rownames",
            "mat.csv.gz"
        )
    )
    # Check that `eval_bare()` call errors on missing object
    expect_error(
        writeCounts(XXX),
        "object 'XXX' not found"
    )
    expect_error(
        writeCounts(seq(1L:10L)),
        "has_dims"
    )
    dir_delete("testcounts")
})
