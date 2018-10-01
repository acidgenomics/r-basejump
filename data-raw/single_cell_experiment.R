# SingleCellExperiment Example Data
# 2018-09-30

library(splatter)
library(tidyverse)

# Restrict to 1 MB per file.
limit <- structure(1e6, class = "object_size")

# Use splatter to generate an example dataset with simulated counts.
# Note: These DE params are natural log scale.
params <- newSplatParams() %>%
    setParam(name = "batchCells", value = 1000) %>%
    setParam(name = "nGenes", value = 200) %>%
    setParam(name = "de.facLoc", value = 1) %>%
    setParam(name = "de.facScale", value = .25) %>%
    setParam(name = "dropout.type", value = "experiment") %>%
    setParam(name = "dropout.mid", value = 3)
sce <- splatSimulate(
    params = params,
    group.prob = c(.5, .5),
    method = "groups"
)

# Sanitize the dimnames into camel case.
sce <- camel(sce, rownames = TRUE, colnames = TRUE)

# Pad the dimnames so they sort correctly.
rownames(sce) <- rownames(sce) %>%
    str_replace("gene", "") %>%
    str_pad(width = 4, side = "left", pad = "0") %>%
    paste0("gene", .)
colnames(sce) <- colnames(sce) %>%
    str_replace("cell", "") %>%
    str_pad(width = 3, side = "left", pad = "0") %>%
    paste0("cell", .)

# Prepare column data.
colData(sce) <- camel(colData(sce))
# Add `sampleID` column. Note that `sampleName` is recommended, but if it is
# not defined, it should be generated from the `sampleID` automatically.
sce$sampleID <- factor(gsub("group", "sample", camel(sce$batch)))
sce$batch <- NULL
sce$cell <- NULL
sce$group <- NULL

# Just slot the raw counts, as a sparse matrix.
counts <- counts(sce)
counts <- as(counts, "sparseMatrix")
assays(sce) <- list(counts = counts)

# Prepare row data.
gr <- makeGRangesFromEnsembl(organism = "Homo sapiens")
gr <- gr[seq_len(nrow(sce))]
# Include only minimal columns.
mcols(gr) <- mcols(gr) %>%
    as("tbl_df") %>%
    select(rowname, broadClass, geneBiotype, geneID, geneName) %>%
    mutate_if(is.factor, droplevels) %>%
    as("DataFrame")
rowRanges(sce) <- gr

# Stash minimal metadata.
metadata(sce) <- list(date = Sys.Date())

# Report the size of each slot in bytes.
vapply(
    X = coerceS4ToList(sce),
    FUN = object.size,
    FUN.VALUE = numeric(1L)
)
stopifnot(object.size(sce) < limit)
stopifnot(validObject(sce))

sce_small <- sce
devtools::use_data(sce_small, compress = "xz", overwrite = TRUE)
