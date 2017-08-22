devtools::load_all()
sparse <- Matrix(
    rbinom(1000L, size = 1L, prob = 0.5),
    ncol = 10L,
    sparse = TRUE)
save(sparse,
     file = file.path(testDataDir, "sparse.rda"),
     compress = "xz")
