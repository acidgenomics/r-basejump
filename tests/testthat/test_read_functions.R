context("Read Functions")

# loadData =====================================================================
test_that("loadData", {
    loaded <- loadData(mtcars)
    rm(mtcars)
    expect_identical(
        loaded,
        c(mtcars = path_join(c(path_real("."), "mtcars.rda")))
    )
    expect_message(
        suppressWarnings(loadData(mtcars)),
        paste("Loading mtcars.rda from", path_real("."))
    )
})

test_that("loadData : Multiple objects in single file", {
    expect_error(
        loadData(multi),
        "is_a_string : loaded has length 2, not 1."
    )
})

test_that("loadData : Already exists", {
    mtcars <- datasets::mtcars
    expect_error(
        loadData(mtcars),
        "Already exists in environment: mtcars"
    )
})

test_that("loadData : Invalid arguments", {
    expect_error(
        loadData(mtcars, dir = "XXX"),
        "is_dir : "
    )
    expect_error(
        loadData(mtcars, envir = "XXX"),
        "is_environment : envir"
    )
})

test_that("loadData : Renamed file", {
    expect_error(
        loadData(renamed),
        "are_identical : name and loaded are not identical."
    )
})



# loadDataAsName ===============================================================
test_that("loadDataAsName : Dot object key-value pair method", {
    loaded <- loadDataAsName(
        newName1 = "mtcars",
        newName2 = "starwars",
        dir = "."
    )
    expect_is(loaded, "character")
    expect_true(exists("newName1", inherits = FALSE))
    expect_true(exists("newName2", inherits = FALSE))
})

test_that("Missing files", {
    expect_error(
        loadDataAsName(newName = "mtcars.rda"),
        "is_existing_file : "
    )
    expect_error(
        loadDataAsName(newName = "XXXXXX"),
        "is_existing_file : "
    )
})

test_that("loadDataAsName : Legacy named character method", {
    loaded <- loadDataAsName(c(test = "mtcars"))
    expect_is(loaded, "character")
    expect_true(exists("test", inherits = FALSE))
})

test_that("Multiple objects in single file", {
    expect_error(
        loadDataAsName(newName = "multi"),
        "is_a_string : loaded has length 2, not 1."
    )
})

test_that("loadDataAsName : Invalid arguments", {
    expect_error(
        loadDataAsName(newName = "mtcars", dir = "XXX"),
        "is_dir : "
    )
    expect_error(
        loadDataAsName(newName = "mtcars", envir = "XXX"),
        "is_environment : envir"
    )
})



# loadRemoteData ===============================================================
test_that("loadRemoteData", {
    loaded <- loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/"))
    expect_is(loaded, "matrix")
    expect_identical(
        loaded["url", "mtcars", drop = TRUE],
        paste(cacheURL, "mtcars.rda", sep = "/")
    )

    # Object already exists in environment =====================================
    mtcars <- datasets::mtcars
    expect_error(
        loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/")),
        "Already exists in environment: mtcars"
    )

    # Invalid arguments ========================================================
    expect_error(
        loadRemoteData(paste(cacheURL, "mmusculus.gtf", sep = "/")),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "is_matching_regex : url does not match"
    )
    expect_error(
        loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/"), envir = "XXX"),
        "is_environment : envir"
    )
})



# localOrRemoteFile ============================================================
test_that("localOrRemoteFile : Vectorized", {
    urls <- paste(cacheURL, c("mtcars.csv", "mtcars.rda"), sep = "/")
    files <- localOrRemoteFile(urls)
    expect_is(files, "fs_path")
    expect_identical(names(files), basename(urls))
})

test_that("localOrRemoteFile : Missing file", {
    expect_error(
        localOrRemoteFile("XXX.csv"),
        "is_existing_file :"
    )
    expect_warning(
        localOrRemoteFile("XXX.csv", severity = "warning"),
        "is_existing_file :"
    )
    expect_identical(
        suppressWarnings(
            localOrRemoteFile("XXX.csv", severity = "warning")
        ),
        NULL
    )
})



# readFileByExtension ==========================================================
test_that("Comma separated value file (.csv)", {
    csv <- readFileByExtension("mtcars.csv")
    expect_is(csv, "tbl_df")
})

test_that("MatrixMarket file (.mtx)", {
    mtx <- readFileByExtension("sparse.mtx")
    expect_is(mtx, "ngTMatrix")
    col <- readFileByExtension("test.colnames")
    expect_identical(col, c("foo", "bar"))
    # rownames use the same code base as colnames
})

test_that("Tab separated values file (.tsv)", {
    tsv <- readFileByExtension("mtcars.tsv")
    expect_is(tsv, "tbl_df")
})

test_that("Table format file (.txt)", {
    txt <- readFileByExtension("mtcars.txt")
    expect_is(txt, "data.frame")
    # txt has integer columns whereas mtcars doesn't
    expect_equal(txt, mtcars)
})

test_that("Excel file (.xlsx)", {
    xlsx <- readFileByExtension("mtcars.xlsx")
    expect_is(xlsx, "tbl_df")
})

test_that("Counts file (.counts)", {
    counts <- readFileByExtension("test.counts")
    expect_is(counts, "matrix")
    expect_identical(
        rownames(counts)[1L:5L],
        c("ENSMUSG00000102693",
          "ENSMUSG00000064842",
          "ENSMUSG00000051951",
          "ENSMUSG00000102851",
          "ENSMUSG00000103377")
    )
})

test_that("Unsupported files", {
    # Missing extension
    expect_error(
        readFileByExtension("DESCRIPTION"),
        "is_matching_regex :"
    )
    # R Data
    expect_error(
        readFileByExtension("mtcars.rda"),
        "Unsupported file extension"
    )
})



# readGFF ======================================================================
test_that("Mouse", {
    mouse <- readGFF("mmusculus.gtf")
    # Check for 9 columns
    expect_identical(ncol(mouse), 9L)
})

test_that("Fruitfly", {
    fruitfly <- readGFF("dmelanogaster.gtf")
    # Check for 9 columns
    expect_identical(ncol(fruitfly), 9L)
})

test_that("Invalid files", {
    expect_error(
        readGFF("mtcars.rda"),
        "GFF/GTF file failed to load"
    )
    expect_error(
        readGFF("mtcars.tsv"),
        "GFF/GTF file failed to load"
    )
})



# readYAML =====================================================================
test_that("bcbio project summary", {
    yaml <- readYAML("summary.yaml")
    expect_identical(
        class(yaml),
        "list"
    )
    expect_identical(
        names(yaml),
        c("date", "upload", "bcbio_system", "samples")
    )
})

test_that("Unsupported file type", {
    expect_error(
        readYAML("mtcars.csv"),
        "is_matching_regex : object"
    )
})

test_that("Missing file", {
    expect_error(
        readYAML("foobar.yaml"),
        "is_existing_file"
    )
})
