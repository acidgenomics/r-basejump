context("readFileByExtension")

test_that("Comma separated value file (.csv)", {
    csv <- readFileByExtension("mtcars.csv")
    expect_is(csv, "tbl_df")
})

test_that("MatrixMarket file (.mtx)", {
    mtx <- readFileByExtension("sparse.mtx")
    expect_is(mtx, "ngTMatrix")
    col <- readFileByExtension("test.colnames")
    expect_identical(col, c("foo", "bar"))
    # rownames use the same code base as colnames
})

test_that("Tab separated values file (.tsv)", {
    tsv <- readFileByExtension("mtcars.tsv")
    expect_is(tsv, "tbl_df")
})

test_that("Table format file (.txt)", {
    txt <- readFileByExtension("mtcars.txt")
    expect_is(txt, "data.frame")
    # txt has integer columns whereas mtcars doesn't
    expect_equal(txt, mtcars)
})

test_that("Excel file (.xlsx)", {
    xlsx <- readFileByExtension("mtcars.xlsx")
    expect_is(xlsx, "tbl_df")
})

test_that("Counts file (.counts)", {
    counts <- readFileByExtension("test.counts")
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

test_that("Unsupported files", {
    # Missing extension
    expect_error(
        readFileByExtension("DESCRIPTION"),
        "is_matching_regex :"
    )
    # R Data
    expect_error(
        readFileByExtension("mtcars.rda"),
        "Unsupported file extension"
    )
})
