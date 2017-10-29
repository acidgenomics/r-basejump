context("readFileByExtension")

test_that("readFileByExtension", {
    # Comma separated value (.csv) file
    csv <- readFileByExtension(
        "http://basejump.seq.cloud/mtcars.csv",
        quiet = TRUE)
    expect_true(tibble::is_tibble(csv))

    # MatrixMarket (.mtx) file
    sparse <- readFileByExtension(
        "http://basejump.seq.cloud/sparse.mtx",
        quiet = TRUE)
    expect_true(is(sparse, "ngTMatrix"))

    # MatrixMarket support file (.colnames)
    colnames <- readFileByExtension(
        "http://basejump.seq.cloud/test.colnames",
        quiet = TRUE)
    expect_equal(
        colnames,
        c("foo", "bar")
    )

    # Tab separated values (.tsv) file
    tsv <- readFileByExtension(
        "http://basejump.seq.cloud/mtcars.tsv",
        quiet = TRUE)
    expect_true(tibble::is_tibble(tsv))

    # Table format (.txt) file
    txt <- readFileByExtension(
        "http://basejump.seq.cloud/mtcars.txt",
        quiet = TRUE)
    expect_equal(
        txt,
        mtcars
    )

    # Excel (.xlsx) file
    xlsx <- readFileByExtension(
        "http://basejump.seq.cloud/mtcars.xlsx",
        quiet = TRUE)
    expect_true(tibble::is_tibble(tsv))

    # Counts (.counts) file
    counts <- readFileByExtension(
        "http://basejump.seq.cloud/test.counts",
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
            "http://basejump.seq.cloud/mtcars.rda",
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
