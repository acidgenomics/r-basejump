# Gene-level RangedSummarizedExperiment example
# 2018-12-11

library(pryr)
library(DESeq2)
library(tidyverse)

# Restrict to 1 MB.
# Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(2e6, class = "object_size")

organism <- "Homo sapiens"
release <- 92L

# Generate example DESeqDataSet using DESeq2.
# Note that we're using simulated counts here.
dds <- makeExampleDESeqDataSet(n = 500L, m = 12L, betaSD = 1L)
object_size(dds)
stopifnot(object_size(dds) < limit)
validObject(dds)

# Coerce to RangedSummarizedExperiment.
# Need to change rows to actual gene identifiers here, and slot colData.
rse <- as(dds, "RangedSummarizedExperiment")
object_size(rse)
stopifnot(object_size(rse) < limit)

# Pad the dimnames so they sort correctly.
rse <- autopadZeros(rse, rownames = TRUE)

# Row data. Include real `geneID`, `geneName` columns to test mapping functions.
rowRanges <- makeGRangesFromEnsembl(organism = organism, release = release)
# Subset to match the number of rows in the example.
rowRanges <- rowRanges[
    i = seq_len(nrow(rse)),
    j = c("geneID", "geneName", "geneBiotype", "broadClass", "entrezID")
]
# Relevel the factor columns, to save disk space.
rowRanges <- relevelRowRanges(rowRanges)
# Note that we're keeping the original rownames from dds_small, and they won't
# match the `geneID` column in rowRanges. This is intentional, for unit testing.
names(rowRanges) <- rownames(rse)
rowRanges(rse) <- rowRanges

# Metadata.
# Stash the date.
metadata(rse)[["date"]] <- Sys.Date()
# Define the interesting groups.
interestingGroups(rse) <- "condition"

# Size check.
vapply(
    X = coerceS4ToList(rse),
    FUN = object_size,
    FUN.VALUE = numeric(1L)
)
object_size(rse)
stopifnot(object_size(rse) < limit)
validObject(rse)

usethis::use_data(rse, compress = "xz", overwrite = TRUE)
