library(Matrix)

sparse <- Matrix(
    rbinom(1000L, size = 1L, prob = 0.5),
    ncol = 10L,
    sparse = TRUE)

extdataDir <- "inst/extdata"
saveData(sparse, dir = extdataDir, compress = "xz")
writeMM(sparse, file = file.path(extdataDir, "sparse.mtx"))
