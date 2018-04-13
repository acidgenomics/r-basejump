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
tbl <- as(df, "tibble")
dgc <- as(mat, "dgCMatrix")

# sampleData
sd <- DataFrame(
    "genotype" = factor(c("wt", "ko", "wt", "ko")),
    "batch" = factor(c(1L, 1L, 2L, 2L)),
    # not a factor yet
    "day" = c(14L, 14L, 30L, 30L),
    row.names = c("sample_1", "sample_2", "sample_3", "sample_4")
)

ensemblRelease <- 87L
ensemblURL <- "ftp://ftp.ensembl.org/pub/release-89"
