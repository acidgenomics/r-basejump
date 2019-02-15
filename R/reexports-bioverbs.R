# In use by bcbioRNASeq v0.2.9.
#' @importFrom bioverbs plotDEGHeatmap
#' @export
bioverbs::plotDEGHeatmap

# Now recommending `plotCounts()` instead of `plotGene()`.
# Still in use by some revdeps, so keep re-exported.
#' @importFrom bioverbs plotGene
#' @export
bioverbs::plotGene
