library(devtools)
library(pryr)
library(DESeq2)

# DESeqDataSet coercion
dds <- makeExampleDESeqDataSet()
rse_dds <- as(dds, "RangedSummarizedExperiment")
object_size(rse_dds)

use_data(rse_dds, compress = "xz", overwrite = TRUE)
