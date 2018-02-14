library(bcbioRNASeq)
load(system.file(
    file.path("extdata", "bcb.rda"),
    package = "bcbioRNASeq"))
counts <- counts(bcb)
save(counts, file = "~/Desktop/counts.rda", compress = "xz")
