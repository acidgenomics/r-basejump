context("Read/Write Functions")



# assignAndSaveData ============================================================
test_that("assignAndSaveData", {
    expect_identical(
        assignAndSaveData(name = "example", object = rnaseq_counts),
        c(example = file.path(getwd(), "example.rda"))
    )
    expect_message(
        assignAndSaveData("example", rnaseq_counts),
        paste("Saving example to", getwd())
    )
    unlink("example.rda")
})



# loadData =====================================================================
test_that("loadData", {
    # rda
    x <- loadData(gr)
    expect_identical(
        x,
        c(gr = normalizePath("gr.rda", winslash = "/"))
    )

    # rds
    x <- loadData(serialized)
    expect_identical(
        x,
        c(serialized = normalizePath("serialized.rds", winslash = "/"))
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
    expect_identical(x, c(example = url))
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
    x <- readFileByExtension("example.gtf")
    expect_s4_class(x, "GRanges")

    x <- readFileByExtension("example.gff3")
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
    file <- "test_read_write_functions.R"
    expect_message(
        readFileByExtension(file),
        "Importing as source code lines"
    )
    x <- readFileByExtension(file)
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
test_that("readGFF : Minimal GFF3", {
    x <- readGFF("example.gff3")
    expect_s4_class(x, "GRanges")
    expect_identical(levels(seqnames(x)), "1")
    expect_identical(
        colnames(mcols(x)),
        c(
            "source",
            "type",
            "score",
            "phase",
            "ID",
            "Alias",
            "external_name",
            "logic_name",
            "Name",
            "biotype",
            "description",
            "gene_id",
            "havana_gene",
            "havana_version",
            "version",
            "Parent",
            "havana_transcript",
            "tag",
            "transcript_id",
            "transcript_support_level",
            "constitutive",
            "ensembl_end_phase",
            "ensembl_phase",
            "exon_id",
            "rank",
            "ccdsid",
            "protein_id"
        )
    )
})

test_that("readGFF : Minimal GTF", {
    x <- readGFF("example.gtf")
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



# saveData =====================================================================
test_that("saveData", {
    dir <- "example"
    paths <- file.path(
        getwd(),
        "example",
        c("rnaseq_counts.rda", "single_cell_counts.rda")
    )
    names(paths) <- c("rnaseq_counts", "single_cell_counts")

    # rda (default)
    x <- saveData(
        rnaseq_counts, single_cell_counts,
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(x, paths)

    # rds
    x <- saveData(
        rnaseq_counts, single_cell_counts,
        ext = "rds",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(
        basename(x),
        c("rnaseq_counts.rds", "single_cell_counts.rds")
    )

    # Check `overwrite = FALSE` mode
    expect_warning(
        saveData(
            rnaseq_counts, single_cell_counts,
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
        saveData(rnaseq_counts, dir = NULL),
        "is_a_string : dir"
    )
})



# transmit =====================================================================
test_that("transmit", {
    x <- transmit(
        remoteDir = ensemblURL,
        pattern = "README",
        compress = FALSE
    )
    y <- file.path(getwd(), "README")
    names(y) <- "README"
    expect_identical(x, y)

    # Check that function skips on existing
    expect_message(
        transmit(
            remoteDir = ensemblURL,
            pattern = "README",
            compress = FALSE
        ),
        "All files have already downloaded"
    )

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
