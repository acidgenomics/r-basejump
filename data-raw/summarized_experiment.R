# SummarizedExperiment example objects, using simulated DESeq2 counts
# Last updated 2018-09-02

library(DESeq2)

# Restrict to 1 MB per file.
mb <- structure(1e6, class = "object_size")

# dds_small ====================================================================
# Generate example DESeqDataSet using DESeq2.
dds <- makeExampleDESeqDataSet(n = 100L, m = 4L)
stopifnot(object.size(dds) < mb)
validObject(dds)
dds_small <- dds
saveData(dds_small, dir = "tests/testthat", compress = "xz")

# rse_small ====================================================================
# Coerce to RangedSummarizedExperiment.
# Need to change rows to actual gene identifiers here, and slot colData.
rse <- as(dds, "RangedSummarizedExperiment")
stopifnot(object.size(rse) < mb)

# Column data.
# Add required `sampleName` column.
rse$sampleName <- as.factor(colnames(rse))
stopifnot(object_size(rse) < mb)

# Row data.
gr <- makeGRangesFromEnsembl("Homo sapiens", release = 92L)
# Subset to match the number of rows in the example.
gr <- gr[seq_len(nrow(rse))]
# Report the size of the ranges.
format(object.size(gr), units = "auto")
# Update the rownames of the object to match our genomic ranges.
rownames(rse) <- names(gr)
rowRanges(rse) <- gr
# Report the size of each slot in bytes.
vapply(
    X = flatFiles(rse),
    FUN = object.size,
    FUN.VALUE = numeric(1L)
)
# Stop if the object is too big.
# stopifnot(object_size(rse) < mb)
validObject(rse)
rse_small <- rse
devtools::use_data(rse_small, compress = "xz", overwrite = TRUE)
