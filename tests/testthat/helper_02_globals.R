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
        c("sample_1", "sample_2", "sample_3", "sample_4")
    )
)
df <- as.data.frame(mat)
tbl <- as(df, "tibble")
dgc <- as(mat, "dgCMatrix")

coldata <- DataFrame(
    "genotype" = factor(c("wt", "ko", "wt", "ko")),
    "batch" = factor(c(1L, 1L, 2L, 2L)),
    row.names = c("sample_1", "sample_2", "sample_3", "sample_4")
)

ensemblRelease <- 87L
ensemblURL <- "ftp://ftp.ensembl.org/pub/release-89"

groceries <- c(NA, NA, "milk", "eggs", "eggs", "veggies")
mpgString <- "18.1, 18.7, 21, 21.4, 22.8"
