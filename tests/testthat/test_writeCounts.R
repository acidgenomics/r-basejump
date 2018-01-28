context("writeCounts")

test_that("writeCounts", {
    df <- as.data.frame(mtcars)
    dgc <- as(mat, "dgCMatrix")
    mat <- as.matrix(mtcars)
    expect_message(
        writeCounts(df, dgc, mat, dir = "counts"),
        "Writing df, dgc, mat"
    )
    expect_identical(
        dir("counts"),
        c(
            "df.csv.gz",
            "dgc.mtx.colnames",
            "dgc.mtx.gz",
            "dgc.mtx.rownames",
            "mat.csv.gz"
        )
    )
    expect_error(
        writeCounts(XXX),
        "object 'XXX' not found"
    )
    expect_error(
        writeCounts(seq(1L:10L)),
        "Object must support `dim\\(\\)`"
    )
    unlink("counts", recursive = TRUE)
})
