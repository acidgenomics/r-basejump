context("writeCounts")

test_that("writeCounts", {
    df <- as.data.frame(mtcars)
    mat <- as.matrix(mtcars)
    dgc <- as(mat, "dgCMatrix")
    expect_message(writeCounts(df, mat, dgc))
    expect_error(
        writeCounts(XXX),
        "object 'XXX' not found"
    )
    expect_error(
        writeCounts(seq(1L:10L)),
        "Object must support dim()"
    )
})
