context("readFileByExtension")

test_that("Comma separated value (.csv) file", {
    csv <- readFileByExtension("http://basejump.seq.cloud/mtcars.csv")
    expect_is(csv, "tbl_df")
})

test_that("MatrixMarket (.mtx) file", {
    sparse <- readFileByExtension("http://basejump.seq.cloud/sparse.mtx")
    expect_is(sparse, "ngTMatrix")

    colnames <- readFileByExtension("http://basejump.seq.cloud/test.colnames")
    expect_identical(colnames, c("foo", "bar"))
    # rownames use the same code base as colnames
})

test_that("Tab separated values (.tsv) file", {
    tsv <- readFileByExtension("http://basejump.seq.cloud/mtcars.tsv")
    expect_is(tsv, "tbl_df")
})

test_that("Table format (.txt) file", {
    txt <- readFileByExtension("http://basejump.seq.cloud/mtcars.txt")
    expect_is(txt, "data.frame")
    # txt has integer columns whereas mtcars doesn't
    expect_equal(txt, mtcars)
})

test_that("Excel (.xlsx) file", {
    xlsx <- readFileByExtension("http://basejump.seq.cloud/mtcars.xlsx")
    expect_is(xlsx, "tbl_df")

    # Counts (.counts) file
    counts <- readFileByExtension("http://basejump.seq.cloud/test.counts")
    expect_is(counts, "matrix")
    expect_identical(
        rownames(counts)[1L:5L],
        c("ENSMUSG00000102693",
          "ENSMUSG00000064842",
          "ENSMUSG00000051951",
          "ENSMUSG00000102851",
          "ENSMUSG00000103377")
    )
})

test_that("R Data (.rda) file (unsupported)", {
    expect_error(
        readFileByExtension("http://basejump.seq.cloud/mtcars.rda"),
        "Unsupported file extension"
    )

    # Missing extension
    expect_error(
        readFileByExtension(
            paste(
                "https://cran.r-project.org",
                "web",
                "packages",
                "testthat",
                "LICENSE",
                sep = "/")),
        "is_matching_regex"
    )
})
