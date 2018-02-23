gene2symbol <- gene2symbol("Homo sapiens", release = 87L)
genes <- head(rownames(gene2symbol), 2L)
x <- data.frame(
    sample1 = c(1L, 2L),
    saple2 = c(3L, 4L),
    row.names = genes,
    stringsAsFactors = FALSE)
