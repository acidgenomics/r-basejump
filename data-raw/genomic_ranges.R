# Genomic Ranges Example
# Last updated 2018-08-27

gr <- makeGRangesFromEnsembl("Homo sapiens", release = 87L)
gr <- head(gr)

saveData(gr, dir = "tests/testthat", compress = "xz")
