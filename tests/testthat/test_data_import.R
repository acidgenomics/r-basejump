context("Data Import and Project Management Utilities")

test_that("packageSE", {
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
    se <- packageSE(mat, colData = coldata, rowData = rowdata)
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
    csvFile <- file.path(testDataURL, "mtcars.csv")
    csv <- readFileByExtension(csvFile)
    expect_true(is_tibble(csv))

    # MatrixMarket file
    mmFile <- file.path(testDataURL, "sparse.mtx")
    sparse <- readFileByExtension(mmFile)
    expect_true(is(sparse, "ngTMatrix"))

    # RData file (unsupported)
    rdaFile <- file.path(testDataURL, "mtcars.rda")
    expect_error(
        readFileByExtension(rdaFile),
        "Unsupported file type")
})



test_that("readYAML", {
    # bcbioRnaseq example YAML file
    yamlFile <- file.path(
        "https://raw.githubusercontent.com",
        "hbc",
        "bcbioRnaseq",
        "master",
        "inst",
        "extra",
        "bcbio",
        "2017-05-23_rnaseq",
        "project-summary.yaml")
    yaml <- readYAML(yamlFile)
    expect_equal(
        names(yaml),
        c("date", "upload", "bcbio_system", "samples"))
})



test_that("transmit", {
    expect_equal(
        transmit("ftp://ftp.ensembl.org/pub/release-89",
                 pattern = "README",
                 rename = "ensembl_readme.txt",
                 compress = TRUE) %>%
            .[["README"]] %>%
            .[[1L]],
        "data-raw/ensembl_readme.txt.gz")
})
