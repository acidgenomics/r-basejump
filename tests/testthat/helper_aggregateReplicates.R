rownames <- c("gene1", "gene2", "gene3", "gene4", "gene5")
mat <- data.frame(
    "sample1_rep1" = c(0L, 0L, 0L, 1L, 2L),
    "sample1_rep2" = c(0L, 0L, 0L, 3L, 4L),
    "sample2_rep1" = c(1L, 2L, 0L, 0L, 0L),
    "sample2_rep2" = c(3L, 4L, 0L, 0L, 0L),
    row.names = rownames) %>%
    as.matrix()
aggmat <- data.frame(
    "sample1" = c(0L, 0L, 0L, 4L, 6L),
    "sample2" = c(4L, 6L, 0L, 0L, 0L),
    row.names = rownames) %>%
    as.matrix()

groupings <- factor(c("sample1", "sample1", "sample2", "sample2"))
names(groupings) <- colnames(mat)
