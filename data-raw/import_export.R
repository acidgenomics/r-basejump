# Import/export file examples
# Last updated 2018-09-26

dir <- "tests/testthat"

example <- colData(rse_small)
rnaseq_counts <- counts(rse_small)
single_cell_counts <- counts(sce_small)

# Compression can cause some AppVeyor CI checks to fail.
saveData(
    example, rnaseq_counts, single_cell_counts,
    ext = "rda", dir = dir, compress = FALSE
)
saveData(
    example, rnaseq_counts, single_cell_counts,
    ext = "rds", dir = dir, compress = FALSE
)

export(example, file = file.path(dir, "example.csv"))
export(example, file = file.path(dir, "example.csv.gz"))
export(example, file = file.path(dir, "example.tsv"))
export(example, file = file.path(dir, "example.tsv.gz"))

write.table(example, file = file.path(dir, "example.txt"))

writeCounts(rnaseq_counts, single_cell_counts, dir = dir)
