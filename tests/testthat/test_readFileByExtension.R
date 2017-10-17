context("readFileByExtension")

test_that("readFileByExtension", {
    # Comma separated value (.csv) file
    csv <- readFileByExtension(
        file.path(testDataURL, "mtcars.csv"),
        quiet = TRUE)
    expect_true(tibble::is_tibble(csv))

    # MatrixMarket (.mtx) file
    sparse <- readFileByExtension(
        file.path(testDataURL, "sparse.mtx"),
        quiet = TRUE)
    expect_true(is(sparse, "ngTMatrix"))

    # MatrixMarket support file (.colnames)
    colnames <- readFileByExtension(
        file.path(testDataURL, "test.colnames"),
        quiet = TRUE)
    expect_equal(
        colnames,
        c("foo", "bar")
    )

    # Tab separated values (.tsv) file
    tsv <- readFileByExtension(
        file.path(testDataURL, "mtcars.tsv"),
        quiet = TRUE)
    expect_true(tibble::is_tibble(tsv))

    # Table format (.txt) file
    txt <- readFileByExtension(
        file.path(testDataURL, "mtcars.txt"),
        quiet = TRUE)
    expect_equal(
        txt,
        mtcars
    )

    # Excel (.xlsx) file
    xlsx <- readFileByExtension(
        file.path(testDataURL, "mtcars.xlsx"),
        quiet = TRUE)
    expect_true(tibble::is_tibble(tsv))

    # Counts (.counts) file
    counts <- readFileByExtension(
        file.path(testDataURL, "test.counts"),
        quiet = TRUE)
    expect_true(is.matrix(counts))
    expect_equal(
        rownames(counts)[1L:5L],
        c("ENSMUSG00000102693",
          "ENSMUSG00000064842",
          "ENSMUSG00000051951",
          "ENSMUSG00000102851",
          "ENSMUSG00000103377")
    )

    # RData (.rda) file (unsupported)
    expect_error(
        readFileByExtension(
            file.path(testDataURL, "mtcars.rda"),
            quiet = TRUE),
        "Unsupported file type"
    )

    # Missing extension
    expect_error(
        readFileByExtension(
            file.path(
                "https://cran.r-project.org",
                "web",
                "packages",
                "testthat",
                "LICENSE"),
            quiet = TRUE),
        "File extension missing"
    )
})
