context("Save Utilities")



# assignAndSaveData ============================================================
test_that("assignAndSaveData", {
    expect_identical(
        assignAndSaveData(name = "example", object = rnaseqCounts),
        c("example" = file.path(getwd(), "example.rda"))
    )
    expect_message(
        assignAndSaveData("example", rnaseqCounts),
        paste("Saving example to", getwd())
    )
    unlink("example.rda")
})



# saveData =====================================================================
test_that("saveData", {
    dir <- "example"
    paths <- file.path(
        getwd(),
        "example",
        c("rnaseqCounts.rda", "singleCellCounts.rda")
    )
    names(paths) <- c("rnaseqCounts", "singleCellCounts")

    # rda (default)
    x <- saveData(
        rnaseqCounts, singleCellCounts,
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(x, paths)

    # rds
    x <- saveData(
        rnaseqCounts, singleCellCounts,
        ext = "rds",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(
        basename(x),
        c("rnaseqCounts.rds", "singleCellCounts.rds")
    )

    # Check `overwrite = FALSE` mode
    expect_warning(
        saveData(
            rnaseqCounts, singleCellCounts,
            dir = dir, overwrite = FALSE
        ),
        "No files were saved."
    )

    unlink(dir, recursive = TRUE)
})

test_that("saveData : Invalid parameters", {
    expect_error(
        saveData(XXX),
        "object 'XXX' not found"
    )
    expect_error(
        saveData("mtcars"),
        "is_name : X"
    )
    expect_error(
        saveData(rnaseqCounts, dir = NULL),
        "is_a_string : dir"
    )
})



# transmit =====================================================================
test_that("transmit : Standard", {
    x <- transmit(
        remoteDir = ensemblURL,
        pattern = "README",
        compress = FALSE
    )
    y <- file.path(getwd(), "README")
    names(y) <- "README"
    expect_identical(x, y)
    unlink("README")
})

test_that("transmit : Rename and compress", {
    x <- transmit(
        remoteDir = ensemblURL,
        pattern = "README",
        rename = "ensembl_readme.txt",
        compress = TRUE
    )
    y <- file.path(getwd(), "ensembl_readme.txt.gz")
    names(y) <- "README"
    expect_identical(x, y)
    unlink("ensembl_readme.txt.gz")
})

test_that("transmit : Invalid parameters", {
    expect_error(
        transmit("http://steinbaugh.com", pattern = "README"),
        "is_matching_regex : remoteDir"
    )
    expect_error(
        transmit("ftp://ftp.wormbase.org/pub/", pattern = "README"),
        "is_non_empty : remoteFiles"
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
    dir <- "example"
    expect_message(
        writeCounts(mat, dgc, dir = dir),
        "Writing mat, dgc"
    )
    expect_identical(
        list.files(dir),
        c(
            "dgc.mtx.gz",
            "dgc.mtx.gz.colnames",
            "dgc.mtx.gz.rownames",
            "mat.csv.gz"
        )
    )
    # Don't allow data.frame
    expect_error(
        writeCounts(mtcars),
        "mtcars is not a matrix"
    )
    # Check that `eval_bare()` call errors on missing object
    expect_error(
        writeCounts(XXX),
        "object 'XXX' not found"
    )
    unlink(dir, recursive = TRUE)
})
