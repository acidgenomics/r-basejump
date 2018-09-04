# SummarizedExperiment example objects, using simulated DESeq2 counts
# Last updated 2018-09-03

library("DESeq2")
library("tidyverse")

# Restrict to 1 MB per file.
mb <- structure(1e6, class = "object_size")

# dds_small ====================================================================
# Generate example DESeqDataSet using DESeq2.
dds <- makeExampleDESeqDataSet(n = 100L, m = 4L)
stopifnot(object.size(dds) < mb)
stopifnot(validObject(dds))
dds_small <- dds
saveData(dds_small, dir = "tests/testthat", compress = "xz")

# rse_small ====================================================================
# Coerce to RangedSummarizedExperiment.
# Need to change rows to actual gene identifiers here, and slot colData.
rse <- as(dds, "RangedSummarizedExperiment")
stopifnot(object.size(rse) < mb)
# Column data.
# Note that `sampleName` column is required for `sampleData()` return.
colData(rse) <- DataFrame(
    sampleName = as.factor(colnames(rse)),
    genotype = factor(
        rep(c("wildtype", "knockout"), times = 2L),
        levels = c("wildtype", "knockout")
    ),
    treatment = factor(
        rep(c("control", "treated"), each = 2L),
        levels = c("control", "treated")
    ),
    row.names = colnames(rse)
)
# Row data.
rowRanges <- makeGRangesFromEnsembl("Homo sapiens", release = 92L)
# Subset to match the number of rows in the example.
rowRanges <- rowRanges[seq_len(nrow(rse))]
# Ensure factor levels in mcols are dropped, to save space.
# Otherwise the example will be too big.
mcols <- mcols(rowRanges) %>%
    as("tbl_df") %>%
    mutate_if(is.factor, droplevels) %>%
    as("DataFrame")
mcols(rowRanges) <- mcols
# Update the rownames of the object to match our genomic ranges.
rownames(rse) <- names(rowRanges)
rowRanges(rse) <- rowRanges
# Define the interesting groups.
interestingGroups(rse) <- c("genotype", "treatment")
# Report the size of each slot in bytes.
vapply(
    X = flatFiles(rse),
    FUN = object.size,
    FUN.VALUE = numeric(1L)
)
stopifnot(object.size(rse) < mb)
stopifnot(validObject(rse))
rse_small <- rse
devtools::use_data(rse_small, compress = "xz", overwrite = TRUE)
