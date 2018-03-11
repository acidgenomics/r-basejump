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
        "is_a_string : loaded has length 2, not 1."
    )
})

test_that("loadData : Renamed file", {
    expect_error(
        loadData(renamed),
        "are_identical : name and loaded are not identical."
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
    x <- loadDataAsName(data_1 = gr, data_2 = makeNames)
    expect_is(x, "character")
    expect_identical(names(x), c("data_1", "data_2"))
    expect_true(exists("data_1", inherits = FALSE))
    expect_true(exists("data_2", inherits = FALSE))
    # Now that the objects are loaded, let's check to make sure we can't
    # accidentally overwrite in the current environment
    expect_error(
        loadDataAsName(data_1 = gr, data_2 = makeNames),
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
        "is_existing_file :"
    )
})

test_that("loadDataAsName : Multiple objects in single file", {
    expect_error(
        loadDataAsName(data = multi),
        "is_a_string : loaded has length 2, not 1."
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
    loaded <- loadRemoteData(paste(cacheURL, "sparse.rda", sep = "/"))
    expect_is(loaded, "dgCMatrix")
    expect_identical(
        loaded["url", "mtcars", drop = TRUE],
        paste(cacheURL, "mtcars.rda", sep = "/")
    )
})

test_that("loadRemoteData : Already exists", {
    mtcars <- datasets::mtcars
    expect_error(
        loadRemoteData(paste(cacheURL, "mtcars.rda", sep = "/")),
        "Already exists in environment: mtcars"
    )
})

test_that("loadRemoteData : Invalid arguments", {
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
    expect_is(files, "character")
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
test_that("readFileByExtension : Comma separated value file (.csv)", {
    csv <- readFileByExtension("mtcars.csv")
    expect_is(csv, "tbl_df")
})

test_that("readFileByExtension : MatrixMarket file (.mtx)", {
    mtx <- readFileByExtension("sparse.mtx")
    expect_is(mtx, "ngTMatrix")
    col <- readFileByExtension("test.colnames")
    expect_identical(col, c("foo", "bar"))
    # rownames use the same code base as colnames
})

test_that("readFileByExtension : Tab separated values file (.tsv)", {
    tsv <- readFileByExtension("mtcars.tsv")
    expect_is(tsv, "tbl_df")
})

test_that("readFileByExtension : Table format file (.txt)", {
    txt <- readFileByExtension("mtcars.txt")
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

test_that("readFileByExtension : Unsupported file type", {
    # Missing extension
    expect_error(
        readFileByExtension("DESCRIPTION"),
        "is_matching_regex :"
    )
    # R Data
    expect_error(
        readFileByExtension("gr.rda"),
        "Unsupported file extension: gr.rda"
    )
})



# readGFF ======================================================================
test_that("readGFF : Mus musculus", {
    x <- readGFF("mmusculus.gtf")
    # Check for 9 columns
    expect_identical(ncol(x), 9L)
})

test_that("readGFF : Drosophila melanogaster", {
    x <- readGFF("dmelanogaster.gtf")
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
        "is_matching_regex : object"
    )
})

test_that("readYAML : Missing file", {
    expect_error(
        readYAML("foobar.yaml"),
        "is_existing_file :"
    )
})
