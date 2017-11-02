library(Matrix)
sparse <- Matrix(
    rbinom(1000L, size = 1L, prob = 0.5),
    ncol = 10L,
    sparse = TRUE)
save(sparse,
     file = "~/Desktop/sparse.rda",
     compress = "xz")
writeMM(sparse, "~/Desktop/sparse.mtx")
