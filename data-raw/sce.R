# Gene-level SingleCellExperiment example
# 2018-11-28

# Splatter params are derived from:
# https://github.com/mikelove/zinbwave-deseq2/blob/master/zinbwave-deseq2.knit.md

library(pryr)
library(splatter)
library(tidyverse)

# Restrict to 2 MB.
# Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(2e6, class = "object_size")

organism <- "Homo sapiens"
release <- 92L

# Use splatter to generate an example dataset with simulated counts.
# Note: These DE params are natural log scale.
params <- newSplatParams() %>%
    setParam(name = "batchCells", value = 100L) %>%
    setParam(name = "nGenes", value = 500L) %>%
    setParam(name = "de.facLoc", value = 1L) %>%
    setParam(name = "de.facScale", value = 0.25) %>%
    # Add more dropout (to test zinbwave weights and DE).
    setParam(name = "dropout.type", value = "experiment") %>%
    setParam(name = "dropout.mid", value = 3L)
sce <- splatSimulate(
    params = params,
    group.prob = c(0.5, 0.5),
    method = "groups"
)

# Sanitize the dimnames into camel case.
sce <- camel(sce, rownames = TRUE, colnames = TRUE)

# Prepare column data.
colData(sce) <- camel(colData(sce))
# Add `sampleID` column. Note that `sampleName` is recommended, but if it is
# not defined, it should be generated from the `sampleID` automatically.
sce$sampleID <- factor(gsub("group", "sample", camel(sce$group)))
sce$batch <- NULL
sce$cell <- NULL
sce$group <- NULL

# Pad the zeros in rows and columns.
# Note that this needs to come after setting up `colData`, otherwise will
# error because `sampleID` column is not defined.
sce <- autopadZeros(sce)

# Just slot the raw counts, as a sparse matrix.
counts <- counts(sce)
counts <- as(counts, "sparseMatrix")
assays(sce) <- list(counts = counts)

# Prepare row data.
rowRanges <- makeGRangesFromEnsembl(organism, release = release)
rowRanges <- rowRanges[
    i = seq_len(nrow(sce)),
    j = c("geneID", "geneName", "geneBiotype", "broadClass", "entrezID")
]
# Relevel the factor columns, to save disk space.
rowRanges <- relevelRowRanges(rowRanges)
# Note that we're keeping the original rownames from dds_small, and they won't
# match the `geneID` column in rowRanges. This is intentional, for unit testing.
names(rowRanges) <- rownames(sce)
rowRanges(sce) <- rowRanges

# Stash minimal metadata.
metadata(sce) <- list(date = Sys.Date())

# Report the size of each slot in bytes.
vapply(
    X = coerceS4ToList(sce),
    FUN = object_size,
    FUN.VALUE = numeric(1L)
)
object_size(sce)
stopifnot(object_size(sce) < limit)
validObject(sce)

usethis::use_data(sce, compress = "xz", overwrite = TRUE)
