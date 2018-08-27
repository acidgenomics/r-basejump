# SummarizedExperiment examples
# Last updated 2018-08-27

library(devtools)
library(pryr)
library(bcbioRNASeq)
library(DESeq2)

# RangedSummarizedExperiment ===================================================
rse_bcb <- as(bcbioRNASeq::bcb_small, "RangedSummarizedExperiment")
assays(rse_bcb) <- assays(rse_bcb)["counts"]
object_size(rse_bcb)

# DESeqDataSet =================================================================
dds <- makeExampleDESeqDataSet()
rse_dds <- as(dds, "RangedSummarizedExperiment")
object_size(rse_dds)

# Save =========================================================================
use_data(
    rse_bcb, rse_dds,
    compress = "xz", overwrite = TRUE
)
