extdataDir <- file.path("inst", "extdata")
dir.create(extdataDir, recursive = TRUE, showWarnings = FALSE)

devtools::install_github("stephenturner/annotables")

grch37 <- annotable(annotables::grch37)
save(
    grch37,
    file = file.path(extdataDir, "grch37.rda"),
    compress = "xz")

grch37Tx2gene <- as.data.frame(annotables::grch37_tx2gene)
rownames(grch37Tx2gene) <- grch37Tx2gene[["enstxp"]]
save(
    grch37Tx2gene,
    file = file.path(extdataDir, "grch37Tx2gene.rda"),
    compress = "xz")
