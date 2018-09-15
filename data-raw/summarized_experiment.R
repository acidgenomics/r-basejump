# SummarizedExperiment example objects
# Last updated 2018-09-15

library("DESeq2")
library("tidyverse")

# Restrict to 1 MB per file.
mb <- structure(1e6, class = "object_size")

# dds_small ====================================================================
# Generate example DESeqDataSet using DESeq2.
# Note that we're using simulated counts here.
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
rowRanges <- makeGRangesFromEnsembl("Homo sapiens")
# Subset to match the number of rows in the example.
rowRanges <- rowRanges[seq_len(nrow(rse))]
# Note that we're keeping the original rownames from dds_small, and they won't
# match the `geneID` column in rowRanges. This is intentional, for unit testing.
names(rowRanges) <- rownames(rse)
# Ensure factor levels in mcols are dropped, to save space.
# Otherwise the example will be too large.
mcols <- mcols(rowRanges) %>%
    as("tbl_df") %>%
    mutate_if(is.factor, droplevels) %>%
    as("DataFrame")
mcols(rowRanges) <- mcols
# Update the rownames of the object to match our genomic ranges.
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

# tx_se_small ==================================================================
tx2gene <- makeTx2geneFromEnsembl("Homo sapiens")
print(tx2gene)
transcriptIDs <- c(
    "ENST00000494424",
    "ENST00000496771",
    "ENST00000612152",
    "ENST00000371584",
    "ENST00000371588",
    "ENST00000413082"
)
stopifnot(all(transcriptIDs %in% rownames(tx2gene)))
sampleIDs <- paste0("sample", seq_len(4L))
counts <- matrix(
    data = seq_len(length(transcriptIDs) * length(sampleIDs)),
    byrow = TRUE,
    nrow = length(transcriptIDs),
    ncol = length(sampleIDs),
    dimnames = list(transcriptIDs, sampleIDs)
)
se <- SummarizedExperiment(
    assays = list(counts = counts),
    rowData = tx2gene[rownames(counts), , drop = FALSE]
)
tx_se_small <- se
devtools::use_data(tx_se_small, compress = "xz", overwrite = TRUE)
