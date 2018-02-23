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



featureGroupings <- factor(c("gene_1", "gene_1", "gene_2", "gene_2"))
names(featureGroupings) <- c("gene_1", "gene_2", "gene_3", "gene_4")

aggMatFeatures <- data.frame(
    sample_1 = c(3L, 7L),
    sample_2 = c(11L, 15L),
    sample_3 = c(19L, 23L),
    sample_4 = c(27L, 31L),
    row.names = c("gene_1", "gene_2")
) %>%
    as.matrix()



replicateGroupings <- factor(c("sample_1", "sample_1", "sample_2", "sample_2"))
names(replicateGroupings) <- c("sample_1", "sample_2", "sample_3", "sample_4")

aggMatReplicates <- data.frame(
    sample_1 = c(6L, 8L, 10L, 12L),
    sample_2 = c(22L, 24L, 26L, 28L),
    row.names = c("gene_1", "gene_2", "gene_3", "gene_4")
) %>%
    as.matrix()
