# RNA-seq count matrices
# Last updated 2018-09-09

library("DESeq2")
library("Matrix")
library("scater")

# RNA-seq counts ===============================================================
dds <- makeExampleDESeqDataSet(n = 100, m = 4)
rnaseq_counts <- counts(dds)

# Single-cell RNA-seq counts ===================================================
single_cell_counts <- scater::sc_example_counts %>%
    as("sparseMatrix") %>%
    .[seq_len(1000L), seq_len(40L)] %>%
    camel(rownames = TRUE, colnames = TRUE)

# Save and write gzip files ====================================================
devtools::use_data(
    rnaseq_counts, single_cell_counts,
    overwrite = TRUE, compress = "xz"
)
writeCounts(
    rnaseq_counts, single_cell_counts,
    dir = "tests/testthat",
    gzip = TRUE
)
