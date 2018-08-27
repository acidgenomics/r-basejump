# RNA-seq count matrices
# Last updated 2018-08-27

library(devtools)
library(DESeq2)
library(Matrix)
library(scater)
load_all()

# RNA-seq counts ===============================================================
dds <- makeExampleDESeqDataSet()
rnaseq_counts <- counts(dds)

# Single-cell RNA-seq counts ===================================================
single_cell_counts <- scater::sc_example_counts %>%
    as("dgCMatrix") %>%
    camel(rownames = TRUE, colnames = TRUE)

# Save and write gzip files ====================================================
use_data(
    rnaseq_counts, single_cell_counts,
    overwrite = TRUE, compress = "xz"
)
writeCounts(
    rnaseq_counts, single_cell_counts,
    dir = "data-raw",
    gzip = TRUE
)
