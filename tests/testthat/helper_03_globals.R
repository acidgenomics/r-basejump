mat <- matrix(
    data = seq(1L:16L),
    nrow = 4L,
    ncol = 4L,
    byrow = FALSE,
    dimnames = list(
        c("gene_1", "gene_2", "gene_3", "gene_4"),
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

ensemblURL <- "ftp://ftp.ensembl.org/pub/release-89"

heatmapList <- c("tree_row", "tree_col", "kmeans", "gtable")
