context("writeCounts")

test_that("writeCounts", {
    df <- as.data.frame(mtcars)
    mat <- as.matrix(mtcars)
    dgc <- as(mat, "dgCMatrix")
    expect_message(
        writeCounts(df, dgc, mat, dir = "testcounts"),
        "Writing df, dgc, mat"
    )
    expect_identical(
        dir("testcounts"),
        c(
            "df.csv.gz",
            "dgc.mtx.colnames",
            "dgc.mtx.gz",
            "dgc.mtx.rownames",
            "mat.csv.gz"
        )
    )
    # Check that `eval_bare()` call errors on missing object
    expect_error(
        writeCounts(XXX),
        "object 'XXX' not found"
    )
    expect_error(
        writeCounts(seq(1L:10L)),
        "has_dims"
    )
    dir_delete("testcounts")
})
