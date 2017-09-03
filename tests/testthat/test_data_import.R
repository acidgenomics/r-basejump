context("Data Import and Project Management Utilities")

test_that("prepareSE", {
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
    se <- prepareSE(mat, colData = coldata, rowData = rowdata)
    expect_equal(
        dim(se),
        c(2L, 2L))
    expect_equal(
        names(metadata(se)),
        c("date", "wd", "sessionInfo"))
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
        readYAML(file.path(testDataDir, "mtcars.csv")),
        "YAML file must have '.yaml' or '.yml' extension")

    # Missing file
    expect_error(
        readYAML("foobar.yaml"),
        "cannot open the connection")
})



test_that("transmit", {
    ftpBaseURL <- "ftp://ftp.ensembl.org/pub/release-89"
    expect_equal(
        transmit(ftpBaseURL,
                 pattern = "README",
                 compress = FALSE) %>%
            .[[pattern]] %>%
            .[[1L]],
        "data-raw/README")
    expect_equal(
        transmit(ftpBaseURL,
                 pattern = "README",
                 rename = "ensembl_readme.txt",
                 compress = TRUE) %>%
            .[[pattern]] %>%
            .[[1L]],
        "data-raw/ensembl_readme.txt.gz")
    expect_error(
        transmit("http://steinbaugh.com",
                 pattern = "README"),
        "FTP protocol not detected")
})
