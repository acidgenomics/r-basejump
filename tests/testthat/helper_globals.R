lapply <- BiocParallel::bplapply
mapply <- BiocParallel::bpmapply

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
        c(
            "sample_1",
            "sample_2",
            "sample_3",
            "sample_4"
        )
    )
)
df <- as.data.frame(mat)
tbl <- as(df, "tbl_df")
dgc <- as(mat, "dgCMatrix")
