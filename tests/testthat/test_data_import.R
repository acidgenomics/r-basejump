context("Data Import and Project Management Utilities")

download.file(
    file.path(testDataURL, "mtcars.rda"),
    "mtcars.rda")

test_that("loadData", {
    x <- loadData(mtcars, dir = getwd())
    expect_equal(
        names(x),
        "mtcars")
    expect_equal(
        basename(x),
        "mtcars.rda")
    expect_error(
        loadData(foobar),
        "foobar missing")
})



test_that("loadDataAsName", {
    x <- loadDataAsName(c(test = "mtcars"), dir = getwd())
    expect_equal(
        names(x),
        "test")
    expect_equal(
        basename(x),
        "mtcars.rda")
    expect_error(
        loadDataAsName(c(test = "foobar")),
        "foobar missing")
})



test_that("loadRemoteData", {
    expect_silent(
        loadRemoteData(file.path(testDataURL, "mtcars.rda")))
    expect_error(
        loadRemoteData(file.path(testDataURL, "mmusculus.gtf")),
        "Data file must contain '.rda' extension")
    expect_error(
        loadRemoteData("foobar.rda"),
        "Remote URL containing '://' required")
})



test_that("readFileByExtension", {
    # Comma separated value (.csv) file
    csv <- file.path(testDataURL, "mtcars.csv") %>%
        readFileByExtension
    expect_true(is_tibble(csv))

    # MatrixMarket (.mtx) file
    sparse <- file.path(testDataURL, "sparse.mtx") %>%
        readFileByExtension
    expect_true(is(sparse, "ngTMatrix"))

    # MatrixMarket support file (.colnames)
    colnames <- file.path(testDataURL, "test.colnames") %>%
        readFileByExtension
    expect_equal(
        colnames,
        c("foo", "bar"))

    # Tab separated values (.tsv) file
    tsv <- file.path(testDataURL, "mtcars.tsv") %>%
        readFileByExtension
    expect_true(is_tibble(tsv))

    # Table format (.txt) file
    txt <- file.path(testDataURL, "mtcars.txt") %>%
        readFileByExtension
    expect_equal(txt, mtcars)

    # Excel (.xlsx) file
    xlsx <- file.path(testDataURL, "mtcars.xlsx") %>%
        readFileByExtension
    expect_true(is_tibble(tsv))

    # Counts (.counts) file
    counts <- file.path(testDataURL, "test.counts") %>%
        readFileByExtension
    expect_true(is.matrix(counts))
    expect_equal(
        rownames(counts)[1L:5L],
        c("ENSMUSG00000102693",
          "ENSMUSG00000064842",
          "ENSMUSG00000051951",
          "ENSMUSG00000102851",
          "ENSMUSG00000103377"))

    # RData (.rda) file (unsupported)
    expect_error(
        readFileByExtension(file.path(testDataURL, "mtcars.rda")),
        "Unsupported file type")

    # Missing extension
    expect_error(
        file.path("https://cran.r-project.org",
                  "web",
                  "packages",
                  "testthat",
                  "LICENSE") %>%
            readFileByExtension,
        "File extension missing")
})



test_that("readYAML", {
    # bcbioRnaseq example YAML file
    yaml <- readYAML(file.path(
        "https://raw.githubusercontent.com",
        "hbc",
        "bcbioRnaseq",
        "master",
        "inst",
        "extra",
        "bcbio",
        "2017-05-23_rnaseq",
        "project-summary.yaml"))
    expect_equal(
        names(yaml),
        c("date", "upload", "bcbio_system", "samples"))

    # Check '.yml' file support
    yaml <- readYAML(file.path(
        "https://raw.githubusercontent.com",
        "steinbaugh",
        "basejump",
        "master",
        ".travis.yml"))
    expect_true("language" %in% names(yaml))

    # Unsupported file type
    expect_error(
        readYAML(file.path(testDataURL, "mtcars.csv")),
        "YAML file must have '.yaml' or '.yml' extension")

    # Missing file
    expect_error(
        readYAML("foobar.yaml"),
        "cannot open the connection")
})



test_that(".SummarizedExperiment", {
    # Create the matrix with invalid names. We'll sanitize these
    # into snake_case later.
    mat <- matrix(
        seq(1L:16L),
        nrow = 4L,
        ncol = 4L,
        dimnames = list(
            c("gene_1", "gene_2", "gene_3", "gene_4"),
            c("sample_1", "sample_2", "sample_3", "sample_4")))
    # Set NA in rowdata here to test retired Ensembl identifier handling
    rowdata <- data.frame(
        ensgene = c("Aaa", "Bbb", "Ccc", NA),
        biotype = c("coding", "coding", "coding", "pseudogene"),
        row.names = rownames(mat))
    coldata <- data.frame(
        genotype = c("wt", "wt", "ko", "ko"),
        age = c(3L, 6L, 3L, 6L),
        row.names = colnames(mat))
    se <- .SummarizedExperiment(
        assays = list(mat),
        rowData = rowdata,
        colData = coldata)

    # Ensure assays requires a list
    expect_error(
        .SummarizedExperiment(
            assays = mat,
            rowData = rowdata,
            colData = coldata),
        "'assays' must be a list")

    expect_equal(
        dim(se),
        c(4L, 4L))
    expect_equal(
        names(metadata(se)),
        c("date",
          "wd",
          "utilsSessionInfo",
          "devtoolsSessionInfo",
          "missingGenes"))
    expect_equal(
        metadata(se)[["missingGenes"]],
        "gene_4")

    # Enforce strict names
    # @seealso [base::make.names()]
    # This checks to see if there are any dashes (invalid) in the names
    expect_error(
        .SummarizedExperiment(
            assays = list(
                mat %>%
                    set_rownames(gsub("_", "-", rownames(mat)))
            ),
            rowData = rowdata,
            colData = coldata),
        "Rownames are not valid.")
    expect_error(
        .SummarizedExperiment(
            assays = list(
                mat %>%
                    set_colnames(gsub("_", "-", colnames(mat)))
            ),
            rowData = rowdata,
            colData = coldata),
        "Colnames are not valid.")

    # Missing rownames
    expect_error(
        .SummarizedExperiment(
            assays = list(
                mat %>%
                    set_rownames(NULL)
            ),
            rowData = rowdata,
            colData = coldata),
        "Assay missing rownames")
    expect_error(
        .SummarizedExperiment(
            assays = list(
                mat %>%
                    set_colnames(NULL)
            ),
            rowData = rowdata,
            colData = coldata),
        "Assay missing colnames")
    expect_error(
        .SummarizedExperiment(
            assays = list(mat),
            rowData = rowdata %>%
                set_rownames(NULL),
            colData = coldata),
        "rowData missing rownames")
    expect_error(
        .SummarizedExperiment(
            assays = list(mat),
            rowData = rowdata,
            colData = coldata %>%
                set_rownames(NULL)),
        "colData missing rownames")

    # Check tibble rownames support
    expect_equal(
        .SummarizedExperiment(
            assays = list(mat),
            rowData = as(rowdata, "tibble"),
            colData = as(coldata, "tibble")),
        se)

    # Duplicate names
    expect_error(
        .SummarizedExperiment(
            assays = list(
                mat %>%
                    set_rownames(c("gene_1",
                                   "gene_1",
                                   "gene_2",
                                   "gene_2"))
            ),
            rowData = rowdata,
            colData = coldata),
        "Non-unique rownames")
    expect_error(
        .SummarizedExperiment(
            assays = list(
                mat %>%
                    set_colnames(c("sample_1",
                                   "sample_1",
                                   "sample_2",
                                   "sample_2"))
            ),
            rowData = rowdata,
            colData = coldata),
        "Non-unique colnames")

    # Bad pass-in of objects not supporting `dim()`
    expect_error(
        .SummarizedExperiment(
            assays = list(c(xxx = "yyy")),
            rowData = rowdata,
            colData = coldata),
        "Assay object must support 'dim\\(\\)'")
    expect_error(
        .SummarizedExperiment(
            assays = list(mat),
            rowData = rowdata,
            colData = c(xxx = "yyy")),
        "colData must support 'dim\\(\\)'")
    expect_error(
        .SummarizedExperiment(
            assays = list(mat),
            rowData = c(xxx = "yyy"),
            colData = coldata),
        "rowData must support 'dim\\(\\)'")

    # Dimension mismatch handling
    expect_error(
        .SummarizedExperiment(
            assays = list(
                cbind(mat, "sample_5" = seq(17L, 20L))
            ),
            rowData = rowdata,
            colData = coldata),
        "colData mismatch with assay slot: sample_5")
    expect_warning(
        .SummarizedExperiment(
            assays = list(
                rbind(mat, "gene_5" = seq(17L, 20L))
            ),
            rowData = rowdata,
            colData = coldata),
        "rowData mismatch with assay slot: gene_5")

    # Bad metadata
    expect_error(
        .SummarizedExperiment(
            assays = list(mat),
            rowData = rowdata,
            colData = coldata,
            metadata = Sys.Date()),
        "Metadata must be 'list' or 'SimpleList' class object")

    # Deprecated
    expect_warning(
        packageSE(
            assays = list(mat),
            colData = coldata,
            rowData = rowdata),
        "Use '.SummarizedExperiment' instead.")
    expect_warning(
        prepareSE(
            assays = list(mat),
            colData = coldata,
            rowData = rowdata),
        "Use '.SummarizedExperiment' instead.")
    expect_warning(
        prepareSummarizedExperiment(
            assays = list(mat),
            colData = coldata,
            rowData = rowdata),
        "Use '.SummarizedExperiment' instead.")
})



test_that("transmit", {
    ensembl <- "ftp://ftp.ensembl.org/pub/release-89"
    expect_equal(
        transmit(ensembl,
                 pattern = "README",
                 compress = FALSE) %>%
            .[["README"]] %>%
            .[[1L]],
        "data-raw/README")
    expect_equal(
        transmit(ensembl,
                 pattern = "README",
                 rename = "ensembl_readme.txt",
                 compress = TRUE) %>%
            .[["README"]] %>%
            .[[1L]],
        "data-raw/ensembl_readme.txt.gz")
    expect_error(
        transmit("http://steinbaugh.com",
                 pattern = "README"),
        "FTP protocol not detected")
    expect_error(
        transmit("ftp://ftp.wormbase.org/pub/",
                 pattern = "README"),
        "No files listed on remote server")
    expect_error(
        transmit(ensembl,
                 pattern = "XXX"),
        "Pattern didn't match any files")
    expect_error(
        transmit(ensembl,
                 pattern = "README",
                 rename = c("XXX", "YYY")),
        "Rename vector doesn't match the number of remote files")
})
