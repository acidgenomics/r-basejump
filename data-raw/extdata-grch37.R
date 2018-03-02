devtools::install_github("stephenturner/annotables")

# FIXME Need to add the legacy code back in
grch37 <- annotable(annotables::grch37)

grch37Tx2gene <- as.data.frame(annotables::grch37_tx2gene)
rownames(grch37Tx2gene) <- grch37Tx2gene[["enstxp"]]

saveData(
    grch37,
    grch37Tx2gene,
    dir = file.path("inst", "extdata"),
    compress = "xz")
