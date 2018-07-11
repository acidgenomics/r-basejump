library(devtools)
library(pryr)
library(bcbioRNASeq)
rse_bcb <- as(bcbioRNASeq::bcb_small, "RangedSummarizedExperiment")
assays(rse_bcb) <- assays(rse_bcb)["counts"]
object_size(rse_bcb)
use_data(rse_bcb, compress = "xz", overwrite = TRUE)
