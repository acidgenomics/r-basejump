# GRanges Example
gr <- makeGRangesFromEnsembl("Homo sapiens", release = 87L)
gr <- head(gr)
saveData(gr, dir = "~", compress = "xz")
