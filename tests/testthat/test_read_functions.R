context("Read Functions")

# loadData =====================================================================
test_that("loadData : Non-standard evaluation", {
    x <- loadData(gr)
    expect_identical(
        x,
        c("gr" = normalizePath("gr.rda", winslash = "/"))
    )
    # Avoid accidentally overwrites in the current environment
    expect_error(
        loadData(gr),
        "Already exists in environment: gr"
    )
})

test_that("loadData : Standard evaluation", {
    expect_error(
        loadData("gr.rda"),
        "is_name :"
    )
})

test_that("loadData : Multiple objects in single file", {
    expect_error(
        loadData(multi),
        "multi.rda contains multiple objects : x, y"
    )
})

test_that("loadData : Renamed file", {
    expect_error(
        loadData(renamed),
        "renamed.rda has been renamed."
    )
})

test_that("loadData : Invalid arguments", {
    expect_error(
        loadData(gr, dir = "XXX"),
        "is_dir :"
    )
    expect_error(
        loadData(gr, envir = "XXX"),
        "is_environment : envir"
    )
})



# loadDataAsName ===============================================================
test_that("loadDataAsName : Non-standard evaluation", {
    x <- loadDataAsName(data_1 = gr, data_2 = mn)
    expect_is(x, "character")
    expect_identical(names(x), c("data_1", "data_2"))
    expect_true(exists("data_1", inherits = FALSE))
    expect_true(exists("data_2", inherits = FALSE))
    # Now that the objects are loaded, let's check to make sure we can't
    # accidentally overwrite in the current environment
    expect_error(
        loadDataAsName(data_1 = gr, data_2 = mn),
        "Already exists in environment: data_1, data_2"
    )
})

test_that("loadData : Standard evaluation", {
    expect_error(
        loadDataAsName(data = "gr.rda"),
        "is_name :"
    )
})

test_that("loadDataAsName : Missing files", {
    expect_error(
        loadDataAsName(data = XXX),
        rdataError
    )
})

test_that("loadDataAsName : Multiple objects in single file", {
    expect_error(
        loadDataAsName(data = multi),
        "multi.rda contains multiple objects : x, y"
    )
})

test_that("loadDataAsName : Invalid arguments", {
    expect_error(
        loadDataAsName(data = gr, dir = "XXX"),
        "is_dir : "
    )
    expect_error(
        loadDataAsName(data = gr, envir = "XXX"),
        "is_environment : envir"
    )
})



# loadRemoteData ===============================================================
test_that("loadRemoteData", {
    x <- loadRemoteData(paste(cacheURL, "rnaseqCounts.rda", sep = "/"))
    # Character matrix of loaded files
    expect_is(x, "character")
    expect_identical(
        x,
        c("rnaseqCounts" = paste(cacheURL, "rnaseqCounts.rda", sep = "/"))
    )
    # Check that the object loaded correctly
    expect_is(rnaseqCounts, "matrix")
})

test_that("loadRemoteData : Already loaded", {
    mtcars <- datasets::mtcars
    expect_error(
        loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/")),
        "Already exists in environment: mtcars"
    )
})

test_that("loadRemoteData : Invalid arguments", {
    expect_error(
        loadRemoteData(paste(cacheURL, "mmusculus.gtf", sep = "/")),
        rdataError
    )
    expect_error(
        loadRemoteData("foobar.rda"),
        "foobar.rda does not match '\\^http"
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
    expect_is(files, "character")
    expect_identical(basename(urls), basename(files))
})

test_that("localOrRemoteFile : Missing file", {
    expect_error(
        localOrRemoteFile("XXX.csv"),
        "is_existing_file :"
    )
})



# readFileByExtension ==========================================================
test_that("readFileByExtension : Comma separated value file (.csv)", {
    x <- readFileByExtension("mtcars.csv")
    expect_is(x, "tbl_df")
})

test_that("readFileByExtension : MatrixMarket file (.mtx)", {
    x <- readFileByExtension("singleCellCounts.mtx.gz")
    expect_is(x, "dgTMatrix")

    x <- readFileByExtension("singleCellCounts.mtx.gz.rownames")
    expect_is(x, "character")

    x <- readFileByExtension("singleCellCounts.mtx.gz.colnames")
    expect_is(x, "character")
})

test_that("readFileByExtension : Tab separated values file (.tsv)", {
    tsv <- readFileByExtension("mtcars.tsv")
    expect_is(tsv, "tbl_df")
})

test_that("readFileByExtension : Table format file (.txt)", {
    expect_warning(readFileByExtension("mtcars.txt"))
    txt <- suppressWarnings(readFileByExtension("mtcars.txt"))
    expect_is(txt, "data.frame")
    # txt has integer columns whereas mtcars doesn't
    expect_equal(txt, mtcars)
})

test_that("readFileByExtension : Excel file (.xlsx)", {
    # Use remote file to check Windows support. Excel files need to be
    # written as binary on Windows to load properly. See `localOrRemoteFile()`
    # for more information.
    xlsx <- readFileByExtension(paste(cacheURL, "mtcars.xlsx", sep = "/"))
    expect_is(xlsx, "tbl_df")
})

test_that("readFileByExtension : Counts file (.counts)", {
    x <- readFileByExtension("test.counts")
    expect_is(x, "matrix")
    expect_identical(
        rownames(x)[1L:5L],
        c(
            "ENSMUSG00000102693",
            "ENSMUSG00000064842",
            "ENSMUSG00000051951",
            "ENSMUSG00000102851",
            "ENSMUSG00000103377"
        )
    )
})

test_that("readFileByExtension : R file", {
    expect_message(
        readFileByExtension("test_read_functions.R"),
        "Importing as source code lines"
    )
    x <- readFileByExtension("test_read_functions.R")
    expect_is(x, "character")
})

test_that("readFileByExtension : Unsupported file type", {
    # Missing extension
    file.create("example")
    expect_error(
        readFileByExtension("example"),
        "is_matching_regex :"
    )
    unlink("example")
})



# readGFF ======================================================================
test_that("readGFF : Drosophila melanogaster", {
    x <- readGFF("dmelanogaster.gtf")
    # Check for 9 columns
    expect_identical(ncol(x), 9L)
})

test_that("readGFF : Mus musculus", {
    x <- readGFF("mmusculus.gtf")
    # Check for 9 columns
    expect_identical(ncol(x), 9L)
})

test_that("readGFF : Unsupported file type", {
    expect_error(
        readGFF("XXX.rda"),
        "is_matching_regex :"
    )
})



# readYAML =====================================================================
test_that("readYAML : bcbio project summary", {
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

test_that("readYAML : Unsupported file type", {
    expect_error(
        readYAML("mtcars.csv"),
        "is_matching_regex : file"
    )
})

test_that("readYAML : Missing file", {
    expect_error(
        readYAML("foobar.yaml"),
        "is_existing_file :"
    )
})
