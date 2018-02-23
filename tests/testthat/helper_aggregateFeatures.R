mat <- data.frame(
    "sample1" = c(0L, 1L, 2L, 3L),
    "sample2" = c(1L, 2L, 3L, 4L),
    row.names = c("gene1.1", "gene1.2", "gene2.1", "gene2.2")) %>%
    as.matrix()
aggmat <- data.frame(
    "sample1" = c(1L, 5L),
    "sample2" = c(3L, 7L),
    row.names = c("gene1", "gene2")) %>%
    as.matrix()

groupings <- factor(c("gene1", "gene1", "gene2", "gene2"))
names(groupings) <- rownames(mat)
