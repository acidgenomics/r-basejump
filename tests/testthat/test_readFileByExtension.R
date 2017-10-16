context("readFileByExtension")

test_that("readFileByExtension", {
    # Comma separated value (.csv) file
    csv <- file.path(testDataURL, "mtcars.csv") %>%
        readFileByExtension()
    expect_true(tibble::is_tibble(csv))

    # MatrixMarket (.mtx) file
    sparse <- file.path(testDataURL, "sparse.mtx") %>%
        readFileByExtension()
    expect_true(is(sparse, "ngTMatrix"))

    # MatrixMarket support file (.colnames)
    colnames <- file.path(testDataURL, "test.colnames") %>%
        readFileByExtension()
    expect_equal(
        colnames,
        c("foo", "bar")
    )

    # Tab separated values (.tsv) file
    tsv <- file.path(testDataURL, "mtcars.tsv") %>%
        readFileByExtension()
    expect_true(tibble::is_tibble(tsv))

    # Table format (.txt) file
    txt <- file.path(testDataURL, "mtcars.txt") %>%
        readFileByExtension()
    expect_equal(
        txt,
        mtcars
    )

    # Excel (.xlsx) file
    xlsx <- file.path(testDataURL, "mtcars.xlsx") %>%
        readFileByExtension
    expect_true(tibble::is_tibble(tsv))

    # Counts (.counts) file
    counts <- file.path(testDataURL, "test.counts") %>%
        readFileByExtension()
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
        file.path(testDataURL, "mtcars.rda") %>%
            readFileByExtension(),
        "Unsupported file type"
    )

    # Missing extension
    expect_error(
        file.path("https://cran.r-project.org",
                  "web",
                  "packages",
                  "testthat",
                  "LICENSE") %>%
            readFileByExtension(),
        "File extension missing"
    )
})
