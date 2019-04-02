context("rankedMatrix")

data(lfc, package = "acidtest", envir = environment())

test_that("Increasing (negative to positive)", {
    x <- rankedMatrix(lfc, method = "increasing")
    y <- matrix(
        data = c(
            1.0, 2.0, 3.0, 4.5, 4.5, 6.0, 7.0, 8.0,
            7.0, 4.5, 2.0, 1.0, 6.0, 4.5, 8.0, 3.0,
            6.0, 8.0, 2.0, 4.5, 3.0, 4.5, 7.0, 1.0,
            8.0, 6.0, 1.0, 7.0, 4.5, 2.0, 4.5, 3.0
        ),
        nrow = 8L,
        ncol = 4L,
        byrow = FALSE,
        dimnames = list(
            paste0("gene", seq_len(8L)),
            paste0("contrast", seq_len(4L))
        )
    )
    expect_is(x, "matrix")
    expect_identical(x, y)
})

test_that("Decreasing (positive to negative)", {
    x <- rankedMatrix(lfc, method = "decreasing")
    y <- matrix(
        data = c(
            8.0, 7.0, 6.0, 4.5, 4.5, 3.0, 2.0, 1.0,
            2.0, 4.5, 7.0, 8.0, 3.0, 4.5, 1.0, 6.0,
            3.0, 1.0, 7.0, 4.5, 6.0, 4.5, 2.0, 8.0,
            1.0, 3.0, 8.0, 2.0, 4.5, 7.0, 4.5, 6.0
        ),
        nrow = 8L,
        ncol = 4L,
        byrow = FALSE,
        dimnames = list(
            paste0("gene", seq_len(8L)),
            paste0("contrast", seq_len(4L))
        )
    )
    expect_is(x, "matrix")
    expect_identical(x, y)
})

test_that("Bidirectional", {
    x <- rankedMatrix(lfc, method = "bidirectional")
    y <- matrix(
        data = c(
            -2.0, -1.0,  0.0,  1.5,  1.5,  3.0,  4.0,  5.0,
             4.0,  1.5, -1.0, -2.0,  3.0,  1.5,  5.0,  0.0,
             3.0,  5.0, -1.0,  1.5,  0.0,  1.5,  4.0, -2.0,
             5.0,  3.0, -2.0,  4.0,  1.5, -1.0,  1.5,  0.0
        ),
        nrow = 8L,
        ncol = 4L,
        byrow = FALSE,
        dimnames = list(
            paste0("gene", seq_len(8L)),
            paste0("contrast", seq_len(4L))
        )
    )
    expect_is(x, "matrix")
    expect_identical(x, y)
})
