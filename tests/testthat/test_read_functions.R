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

test_that("loadData : Duplicate RDA and RDS files", {
    expect_error(
        loadData(example),
        "Duplicates : example.rda, example.rds"
    )
})

test_that("loadData : Uncertain extension", {
    expect_error(
        loadData(gr, serialized),
        "Multiple extensions : rda, rds"
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

test_that("loadDataAsName : Serialized", {
    x <- loadDataAsName(new = serialized)
    expect_identical(names(x), "new")
    expect_true(exists("new", inherits = FALSE))
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
    url <- paste(cacheURL, "example.rds", sep = "/")
    x <- loadRemoteData(url)
    # Character matrix of loaded files
    expect_is(x, "character")
    expect_identical(x, c("example" = url))
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
            paste(paste(cacheURL, "example.rda", sep = "/")),
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
    expect_is(x, "data.frame")
})

test_that("readFileByExtension : GFF", {
    x <- readFileByExtension("mmusculus.gtf")
    expect_s4_class(x, "GRanges")
})

test_that("readFileByExtension : MatrixMarket file (.mtx)", {
    x <- readFileByExtension("single_cell_counts.mtx.gz")
    expect_is(x, "dgTMatrix")

    x <- readFileByExtension("single_cell_counts.mtx.gz.rownames")
    expect_is(x, "character")

    x <- readFileByExtension("single_cell_counts.mtx.gz.colnames")
    expect_is(x, "character")
})

test_that("readFileByExtension : Tab separated values file (.tsv)", {
    x <- readFileByExtension("example.tsv")
    expect_is(x, "data.frame")
})

test_that("readFileByExtension : Excel file (.xlsx)", {
    # Use remote file to check Windows support. Excel files need to be
    # written as binary on Windows to load properly. See `localOrRemoteFile()`
    # for more information.
    x <- readFileByExtension(paste(cacheURL, "mtcars.xlsx", sep = "/"))
    expect_is(x, "data.frame")
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

test_that("readFileByExtension : R script", {
    expect_message(
        readFileByExtension("test_read_functions.R"),
        "Importing as source code lines"
    )
    x <- readFileByExtension("test_read_functions.R")
    expect_is(x, "character")
})

test_that("readFileByExtension : R Data", {
    # rda
    x <- readFileByExtension(paste(cacheURL, "example.rda", sep = "/"))
    expect_is(x, "tbl_df")

    # rds
    x <- readFileByExtension(paste(cacheURL, "example.rds", sep = "/"))
    expect_is(x, "tbl_df")

    # Error on object containing multiple data
    expect_error(
        readFileByExtension(paste(cacheURL, "multi.rda", sep = "/")),
        "File does not contain a single object"
    )
})

test_that("readFileByExtension : YAML", {
    x <- readFileByExtension("example.yml")
    expect_is(x, "list")
})

test_that("readFileByExtension : No extension", {
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
    expect_s4_class(x, "GRanges")
    expect_identical(levels(seqnames(x)), "X")
    expect_identical(
        colnames(mcols(x)),
        c(
            "source",
            "type",
            "score",
            "phase",
            "gene_id",
            "gene_symbol",
            "transcript_id",
            "transcript_symbol"
        )
    )
})

test_that("readGFF : Mus musculus", {
    x <- readGFF("mmusculus.gtf")
    expect_identical(levels(seqnames(x)), "1")
    expect_identical(
        colnames(mcols(x)),
        c(
            "source",
            "type",
            "score",
            "phase",
            "gene_id",
            "gene_version",
            "gene_name",
            "gene_source",
            "gene_biotype",
            "transcript_id",
            "transcript_version",
            "transcript_name",
            "transcript_source",
            "transcript_biotype",
            "transcript_support_level",
            "exon_number",
            "exon_id",
            "exon_version",
            "tag",
            "ccds_id",
            "protein_id",
            "protein_version"
        )
    )
})

test_that("readGFF : Unsupported file type", {
    expect_error(
        readGFF("XXX.rda"),
        "is_matching_regex :"
    )
})



# readJSON =====================================================================
test_that("readJSON", {
    x <- readJSON("example.json")
    expect_is(x, "list")
})



# readYAML =====================================================================
test_that("readYAML : bcbio project summary", {
    x <- readYAML("example.yml")
    expect_is(x, "list")
})
