# Genomic ranges
# Last updated 2018-09-02

gr <- makeGRangesFromEnsembl("Homo sapiens", release = 87L)
saveData(gr, dir = "tests/testthat", compress = "xz")
