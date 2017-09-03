context("Data Import and Project Management Utilities")

test_that("prepareSummarizedExperiment", {
    mat <- mtcars %>%
        .[c("Mazda RX4", "Datsun 710"), ] %>%
        .[, c("mpg", "gear")] %>%
        as.matrix
    coldata <- data.frame(
        description = c("Miles per gallon", "Number of gears"),
        abbreviation = c(TRUE, FALSE),
        row.names = colnames(mat))
    rowdata <- data.frame(
        manufacturer = c("Mazda", "Datsun"),
        model_number = c("RX4", "710"),
        row.names = rownames(mat))
    # Enforce strict names
    # @seealso [base::make.names()].
    expect_error(
        prepareSummarizedExperiment(
            mat,
            colData = coldata,
            rowData = rowdata),
        "Row names are not valid.")
    # After snake_case sanitization, it should now work
    mat <- snake(mat, rownames = TRUE)
    rowdata <- snake(rowdata, rownames = TRUE)
    se <- prepareSummarizedExperiment(
        mat,
        colData = coldata,
        rowData = rowdata)
    expect_equal(
        dim(se),
        c(2L, 2L))
    expect_equal(
        names(metadata(se)),
        c("date", "wd", "sessionInfo"))

    # Check tibble rownames support
    expect_equal(
        prepareSummarizedExperiment(
            mat,
            colData = as(coldata, "tibble"),
            rowData = as(rowdata, "tibble")),
        se)

    # Bad pass-in of objects not supporting `dim()`
    expect_error(
        prepareSummarizedExperiment(
            list(c(xxx = "yyy")),
            coldata,
            rowdata),
        "Assay object must support 'dim\\(\\)'")
    expect_error(
        prepareSummarizedExperiment(
            mat,
            colData = c(xxx = "yyy"),
            rowData = rowdata),
        "colData must support 'dim\\(\\)'")
    expect_error(
        prepareSummarizedExperiment(
            mat,
            colData = coldata,
            rowData = c(xxx = "yyy")),
        "rowData must support 'dim\\(\\)'")

    # Dimension mismatch handling
    matcolmismatch <- cbind(mat, "extra" = c("A", "B"))
    expect_error(
        prepareSummarizedExperiment(
            matcolmismatch,
            colData = coldata,
            rowData = rowdata),
        "colData mismatch with assay slot: extra")
    matrowmismatch <- rbind(mat, "valiant" = c(18.1, 3L))
    expect_warning(
        prepareSummarizedExperiment(
            matrowmismatch,
            colData = coldata,
            rowData = rowdata),
        "rowData mismatch with assay slot: valiant")

    # Deprecations
    expect_warning(
        packageSE(
            mat,
            colData = coldata,
            rowData = rowdata),
        "Use 'prepareSummarizedExperiment' instead")
    expect_warning(
        prepareSE(
            mat,
            colData = coldata,
            rowData = rowdata),
        "Use 'prepareSummarizedExperiment' instead")
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
    # CSV file
    csv <- file.path(testDataURL, "mtcars.csv") %>%
        readFileByExtension
    expect_true(is_tibble(csv))

    # MatrixMarket file
    sparse <- file.path(testDataURL, "sparse.mtx") %>%
        readFileByExtension
    expect_true(is(sparse, "ngTMatrix"))

    # RData file (unsupported)
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
        transmit(ensembl,
                 pattern = "XXX"),
        "Pattern didn't match any files")
    expect_error(
        transmit(ensembl,
                 pattern = "README",
                 rename = c("XXX", "YYY")),
        "Rename vector doesn't match the number of remote files")
})
