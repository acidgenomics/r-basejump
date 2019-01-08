mat <- matrix(
    data = seq(1L:16L),
    nrow = 4L,
    ncol = 4L,
    byrow = FALSE,
    dimnames = list(
        c(
            "ENSG00000000001",
            "ENSG00000000002",
            "ENSG00000000003",
            "ENSG00000000004"
        ),
        paste0("sample", seq_len(4L))
    )
)
sparse <- Matrix::Matrix(mat, sparse = TRUE)
df <- S4Vectors::DataFrame(mat)
tbl <- tibble::as_tibble(mat, rownames = "rowname")
usethis::use_data(mat, sparse, df, tbl, overwrite = TRUE, compress = "xz")
