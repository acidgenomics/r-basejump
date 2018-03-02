load(system.file("extdata/bcb.rda", package = "bcbioRNASeq"))
counts <- bcbioRNASeq::counts(bcb)
saveData(counts, dir = "tests/testthat", compress = "xz")
