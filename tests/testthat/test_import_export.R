context("Import/Export")

data(rse, sce, envir = environment())

mat <- assay(rse)
sparse <- assay(sce)
url <- basejumpCacheURL



# assignAndSaveData ============================================================
test_that("assignAndSaveData", {
    object <- suppressMessages(assignAndSaveData(
        name = "XXX",
        object = rse,
        dir = ".",
        ext = "rds"
    ))
    expect_identical(
        object = object,
        expected = c(XXX = file.path(getwd(), "XXX.rds"))
    )
    unlink("XXX.rds")
})



# import =======================================================================
with_parameters_test_that(
    "import : Comma separated value file (.csv)", {
        object <- import(file = "example.csv", dataFrame = dataFrame)
        expect_is(object, dataFrame)
    },
    dataFrame = .dataFrameChoices
)

with_parameters_test_that(
    "import : Tab separated values file (.tsv)", {
        object <- import(file = "example.tsv", dataFrame = dataFrame)
        expect_is(object, dataFrame)
    },
    dataFrame = .dataFrameChoices
)

test_that("import : Excel file (.xlsx)", {
    # Use remote file to check Windows support. Excel files need to be written
    # as binary on Windows to load properly. See `localOrRemoteFile` for more
    # information.
    file <- paste(url, "example.xlsx", sep = "/")
    object <- import(file = file, dataFrame = "data.frame")
    expect_is(object, "data.frame")
})

test_that("import : GFF3", {
    object <- import("example.gff3")
    expect_s4_class(object, "GRanges")
    expect_identical(levels(seqnames(object)), "1")
    expect_identical(
        object = colnames(mcols(object)),
        expected = c(
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

test_that("import : GTF", {
    object <- import("example.gtf")
    expect_identical(levels(seqnames(object)), "1")
    expect_identical(
        object = colnames(mcols(object)),
        expected = c(
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

test_that("import : MatrixMarket file (.mtx)", {
    object <- import("single_cell_counts.mtx.gz")
    expect_is(object, "sparseMatrix")

    object <- import("single_cell_counts.mtx.gz.rownames")
    expect_is(object, "character")

    object <- import("single_cell_counts.mtx.gz.colnames")
    expect_is(object, "character")
})

test_that("import : Counts file (.counts)", {
    object <- import("example.counts")
    expect_is(object, "matrix")
    expect_identical(
        object = head(rownames(object), n = 5L),
        expected = c(
            "ENSMUSG00000102693",
            "ENSMUSG00000064842",
            "ENSMUSG00000051951",
            "ENSMUSG00000102851",
            "ENSMUSG00000103377"
        )
    )
})

test_that("import : R script", {
    expect_is(
        object = import(file = "example.R"),
        class = "character"
    )
})

test_that("import : R Data", {
    # R data.
    object <- import(paste(url, "example.rda", sep = "/"))
    expect_is(object, "DataFrame")

    # R data serialized.
    object <- import(paste(url, "example.rds", sep = "/"))
    expect_is(object, "DataFrame")

    # Error on object containing multiple data.
    expect_error(
        object = import(paste(url, "multi.rda", sep = "/")),
        regexp = "File does not contain a single object"
    )
})

test_that("import : JSON", {
    object <- import("example.json")
    expect_is(object, "list")
})

test_that("import : YAML", {
    object <- import("example.yml")
    expect_is(object, "list")
})

test_that("import : No extension", {
    # Missing extension.
    file.create("example")
    expect_error(
        object = import("example"),
        regexp = "file extension"
    )
    unlink("example")
})



# loadData =====================================================================
test_that("loadData", {
    # R data.
    expect_identical(
        object = loadData(gr),
        expected = c(gr = realpath("gr.rda"))
    )
    # We're defaulting to global environment.
    expect_true(exists("gr", envir = globalenv(), inherits = FALSE))

    # R data serialized.
    expect_identical(
        object = loadData(serialized),
        expected = c(
            serialized = realpath("serialized.rds")
        )
    )
})

# Don't allow RDS/RDA soup.
test_that("loadData : Mixed extensions", {
    expect_error(
        object = loadData(gr, serialized) %>% basename(),
        regexp = "RDS/RDA/RDATA"
    )
})

test_that("loadData : Standard evaluation", {
    expect_error(
        object = loadData("gr.rda"),
        regexp = "non-standard evaluation"
    )
})

# Avoid accidental reassignment in the current environment.
test_that("loadData : Already exists", {
    gr <- TRUE
    expect_error(
        object = loadData(gr),
        regexp = "reassignment"
    )
})

test_that("loadData : Multiple objects in single file", {
    expect_error(
        object = loadData(multi),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("loadData : Renamed file", {
    expect_error(
        object = loadData(renamed),
        regexp = "renamed.rda has been renamed."
    )
})

test_that("loadData : Duplicate RDA and RDS files", {
    expect_error(
        object = loadData(example),
        regexp = "example is not unique on disk."
    )
})

test_that("loadData : Invalid arguments", {
    expect_error(
        object = loadData(gr, dir = "XXX"),
        regexp = "hasAccess"
    )
    expect_error(
        object = loadData(gr, envir = "XXX"),
        regexp = "is.environment"
    )
})



# loadDataAsName ===============================================================
test_that("loadDataAsName : Non-standard evaluation", {
    object <- loadDataAsName(data_1 = gr, data_2 = mn)
    expect_is(object, "character")
    expect_identical(names(object), c("data_1", "data_2"))
    # We're defaulting to global environment.
    expect_true(exists("data_1", envir = globalenv(), inherits = FALSE))
    expect_true(exists("data_2", envir = globalenv(), inherits = FALSE))
    # Now that the objects are loaded, let's check to make sure we can't
    # accidentally overwrite in the current environment.
    expect_error(
        object = loadDataAsName(data_1 = gr, data_2 = mn),
        regexp = "reassignment"
    )
})


test_that("loadDataAsName : Serialized", {
    object <- loadDataAsName(new = serialized)
    expect_identical(names(object), "new")
    # We're defaulting to global environment.
    expect_true(exists("new", envir = globalenv(), inherits = FALSE))
})

test_that("loadData : Standard evaluation", {
    expect_error(
        object = loadDataAsName(data = "gr.rda"),
        regexp = "non-standard evaluation"
    )
})

test_that("loadDataAsName : Missing files", {
    expect_error(
        object = loadDataAsName(data = XXX),
        regexp = rdataLoadError
    )
})

test_that("loadDataAsName : Multiple objects in single file", {
    expect_error(
        object = loadDataAsName(data = multi),
        regexp = "multi.rda contains multiple objects: x, y"
    )
})

test_that("loadDataAsName : Invalid arguments", {
    expect_error(
        object = loadDataAsName(data = gr, dir = "XXX"),
        regexp = "hasAccess"
    )
    expect_error(
        object = loadDataAsName(data = gr, envir = "XXX"),
        regexp = "is.environment"
    )
})



# loadRemoteData ===============================================================
test_that("loadRemoteData", {
    url <- paste(url, "example.rds", sep = "/")
    object <- loadRemoteData(url)
    # Character matrix of loaded files.
    expect_is(object, "character")
    expect_identical(object, c(example = url))
    # Check that the object loaded correctly.
    expect_is(example, "DataFrame")
})

test_that("loadRemoteData : Already loaded", {
    example <- TRUE
    expect_error(
        object = loadRemoteData(url = paste(url, "example.rda", sep = "/"))
    )
})

test_that("loadRemoteData : Invalid arguments", {
    expect_error(
        object = loadRemoteData(url = paste(url, "mmusculus.gtf", sep = "/")),
        regexp = rdataLoadError
    )
    expect_error(
        object = loadRemoteData("foobar.rda"),
        regexp = "URL"
    )
    expect_error(
        object = loadRemoteData(
            url = paste(paste(url, "example.rda", sep = "/")),
            envir = "XXX"
        ),
        regexp = "is.environment"
    )
})



# localOrRemoteFile ============================================================
test_that("localOrRemoteFile : Vectorized", {
    urls <- paste(url, c("example.csv", "example.rda"), sep = "/")
    files <- localOrRemoteFile(urls)
    expect_is(files, "character")
    expect_identical(basename(urls), basename(files))
})

test_that("localOrRemoteFile : Missing file", {
    expect_error(
        object = localOrRemoteFile("XXX.csv"),
        regexp = "hasAccess"
    )
})



# saveData =====================================================================
test_that("saveData", {
    dir <- "example"
    paths <- file.path(
        getwd(),
        "example",
        c("rse.rda", "sce.rda")
    )
    names(paths) <- c("rse", "sce")

    # R data.
    object <- saveData(
        rse, sce,
        ext = "rda",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(object, paths)

    # R data serialized.
    object <- saveData(
        rse, sce,
        ext = "rds",
        dir = dir,
        overwrite = TRUE
    )
    expect_identical(
        object = basename(object),
        expected = c("rse.rds", "sce.rds")
    )

    # Check `overwrite = FALSE` mode.
    expect_warning(
        object = saveData(
            rse, sce,
            dir = dir, overwrite = FALSE
        ),
        regexp = "No files were saved."
    )

    unlink(dir, recursive = TRUE)
})

test_that("saveData : Invalid parameters", {
    expect_error(
        object = saveData(XXX),
        regexp = "object 'XXX' not found"
    )
    expect_error(
        object = saveData("example"),
        regexp = "non-standard evaluation"
    )
    expect_error(
        object = saveData(rse, dir = NULL),
        regexp = "isString"
    )
})



# transmit =====================================================================
# Note that only FTP is currently supported.
remoteDir <- paste(
    "ftp://ftp.pantherdb.org",
    "sequence_classifications",
    "current_release",
    sep = "/"
)

test_that("transmit", {
    skip_on_travis()

    object <- transmit(
        remoteDir = remoteDir,
        pattern = "README",
        compress = FALSE
    )
    expected <- file.path(getwd(), "README")
    names(expected) <- "README"
    expect_identical(object, expected)

    # Check that function skips on existing.
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

test_that("transmit : Rename and compress", {
    skip_on_travis()

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

# TODO Improve the error messages for these.
test_that("transmit : Invalid parameters", {
    skip_on_travis()
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



# writeCounts ==================================================================
test_that("writeCounts", {
    dir <- "example"
    expect_message(
        object = writeCounts(mat, sparse, dir = dir, compress = TRUE),
        regexp = "Writing mat, sparse"
    )
    expect_identical(
        object = list.files(dir),
        expected = c(
            "mat.csv.gz",
            "sparse.mtx.gz",
            "sparse.mtx.gz.colnames",
            "sparse.mtx.gz.rownames"
        )
    )
    # Require a matrix, and don't allow data frames.
    expect_error(
        object = writeCounts(mtcars),
        regexp = "mtcars is not a matrix"
    )
    # Check that `eval_bare` call errors on missing object.
    expect_error(
        object = writeCounts(XXX),
        regexp = "object 'XXX' not found"
    )
    unlink(dir, recursive = TRUE)
})
