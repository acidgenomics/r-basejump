# FIXME Disable lazy loading.
# data(rse_small, sce_small, tx_se_small, envir = environment())
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
df <- as(mat, "DataFrame")
tbl <- tibble::as_tibble(mat, rownames = "rowname")
sparse <- as(mat, "sparseMatrix")
