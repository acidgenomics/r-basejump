# Genomic ranges
# Last updated 2018-09-06

gr <- makeGRangesFromEnsembl("Homo sapiens", release = 87L)
saveData(gr, dir = "tests/testthat", compress = "xz")
