extdataDir <- file.path("inst", "extdata")
dir.create(extdataDir, recursive = TRUE, showWarnings = FALSE)

library(Matrix)
sparse <- Matrix(
    rbinom(1000L, size = 1L, prob = 0.5),
    ncol = 10L,
    sparse = TRUE)

save(sparse,
     file = file.path(extdataDir, "sparse.rda"),
     compress = "xz")
writeMM(sparse, file.path(extdataDir, "sparse.mtx"))
