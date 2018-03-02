# Example GRanges object
gr <- genes("Homo sapiens", release = 87L, return = "GRanges")
gr <- head(gr)
saveData(gr, dir = "tests/testthat", compress = "xz")
