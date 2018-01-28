devtools::load_all()

library(bcbioRNASeq)
load(system.file(
    file.path("extdata", "bcb.rda"),
    package = "bcbioRNASeq"))
counts <- counts(bcb)

saveData(counts, dir = "~/Desktop")
