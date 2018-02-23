aggMatFeatures <- data.frame(
    sample_1 = c(3L, 7L),
    sample_2 = c(11L, 15L),
    sample_3 = c(19L, 23L),
    sample_4 = c(27L, 31L),
    row.names = c("gene_1", "gene_2")
) %>%
    as.matrix()



aggMatReplicates <- data.frame(
    sample_1 = c(6L, 8L, 10L, 12L),
    sample_2 = c(22L, 24L, 26L, 28L),
    row.names = c("gene_1", "gene_2", "gene_3", "gene_4")
) %>%
    as.matrix()



colData <- DataFrame(
    genotype = c("wt", "ko", "wt", "ko"),
    row.names = c("sample_1", "sample_2", "sample_3", "sample_4"),
    batch = c(1L, 1L, 2L, 2L)
)



ensemblURL <- "ftp://ftp.ensembl.org/pub/release-89"



featureGroupings <- factor(c("gene_1", "gene_1", "gene_2", "gene_2"))
names(featureGroupings) <- c("gene_1", "gene_2", "gene_3", "gene_4")



matrix <- matrix(
    data = seq(1L:16L),
    nrow = 4L,
    ncol = 4L,
    byrow = FALSE,
    dimnames = list(
        c("gene_1", "gene_2", "gene_3", "gene_4"),
        c("sample_1", "sample_2", "sample_3", "sample_4")
    )
)



mpgString <- "18.1, 18.7, 21, 21.4, 22.8"



replicateGroupings <- factor(c("sample_1", "sample_1", "sample_2", "sample_2"))
names(replicateGroupings) <- c("sample_1", "sample_2", "sample_3", "sample_4")



x <- data.frame(
    sample1 = c(1L, 2L),
    sample2 = c(3L, 4L),
    row.names = c("gene1", "gene2"),
    stringsAsFactors = FALSE)



data <- data.frame(
    sample1 = c(1L, 2L),
    sample2 = c(3L, 4L),
    row.names = c("gene1", "gene2"),
    stringsAsFactors = FALSE)



tibble <- tibble(
    sample1 = c(1L, 2L),
    sample2 = c(3L, 4L)
)



vec1 <- seq(from = 1L, to = 5L, by = 1L)
vec2 <- vec1 ^ 2L
means <- c(vec1 = 2.605171, vec2 = 6.786916)



fc <- c(-8L, -4L, -2L, 1L, 2L, 4L, 8L)
lr <- seq(-3L, 3L, 1L)



plotNames <- c("tree_row", "tree_col", "kmeans", "gtable")
