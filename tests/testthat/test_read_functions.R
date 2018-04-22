context("Read Functions")

# loadData =====================================================================
test_that("loadData", {
    # rda
    x <- loadData(gr)
    expect_identical(
        x,
        c("gr" = normalizePath("gr.rda", winslash = "/"))
    )

    # rds
    x <- loadData(serialized)
    expect_identical(
        x,
        c("serialized" = normalizePath("serialized.rds", winslash = "/"))
    )
})

test_that("loadData : Standard evaluation", {
    expect_error(
        loadData("gr.rda"),
        "is_name :"
    )
})

test_that("loadData : Already exists", {
    # Avoid accidentally overwrites in the current environment
    gr <- TRUE
    expect_error(
        loadData(gr),
        "Already exists in environment: gr"
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

test_that("loadData : RDA and RDS files in directory", {
    expect_error(
        loadData(example),
        "Duplicates : example.rda, example.rds"
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
    x <- loadRemoteData(paste(cacheURL, "example.rds", sep = "/"))
    # Character matrix of loaded files
    expect_is(x, "character")
    expect_identical(
        x,
        c("example" = paste(cacheURL, "example.rds", sep = "/"))
    )
    # Check that the object loaded correctly
    expect_is(example, "data.frame")
})

test_that("loadRemoteData : Already loaded", {
    example <- TRUE
    expect_error(
        loadRemoteData(paste(cacheURL, "example.rda", sep = "/")),
        "Already exists in environment: example"
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
        loadRemoteData(
            paste(cacheURL, "example.rda", sep = "/"),
            envir = "XXX"
        ),
        "is_environment : envir"
    )
})



# localOrRemoteFile ============================================================
test_that("localOrRemoteFile : Vectorized", {
    urls <- paste(cacheURL, c("example.csv", "example.rda"), sep = "/")
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
    x <- readFileByExtension("example.csv")
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
    tsv <- readFileByExtension("example.tsv")
    expect_is(tsv, "tbl_df")
})

test_that("readFileByExtension : Table format file (.txt)", {
    expect_warning(readFileByExtension("example.txt"))
    txt <- suppressWarnings(readFileByExtension("example.txt"))
    expect_is(txt, "data.frame")
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
    x <- readFileByExtension("example.counts")
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
    x <- readYAML("example.yaml")
    expect_is(x, "list")
})
