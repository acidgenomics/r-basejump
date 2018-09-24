# SingleCellExperiment Example Data
# 2018-09-24

library(splatter)
library(tidyverse)

organism <- "Homo sapiens"
release <- 92L

# Use splatter to generate an example dataset with simulated counts.
# Note: These DE params are natural log scale.
params <- newSplatParams()
params <- setParam(params, name = "batchCells", value = c(100, 100))
params <- setParam(params, name = "nGenes", value = 100)
params <- setParam(params, name = "de.facLoc", value = 1)
params <- setParam(params, name = "de.facScale", value = .25)
params <- setParam(params, name = "dropout.type", value = "experiment")
params <- setParam(params, name = "dropout.mid", value = 3)

sce <- splatSimulate(params, group.prob = c(.5, .5), method = "groups")

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
# Add sampleID and sampleName columns.
sce$batch <- factor(camel(sce$batch))
sce$sampleID <- factor(gsub("group", "sample", camel(sce$group)))
sce$group <- NULL
sce$cell <- NULL

# Just keep raw counts.
assays(sce) <- assays(sce)["counts"]

# Prepare row data.
gr <- makeGRangesFromEnsembl(organism = organism, release = release)
rowRanges(sce) <- gr[seq_len(nrow(sce))]

# Stash minimal metadata.
metadata(sce) <- list(date = Sys.Date())

sce_small <- sce
devtools::use_data(sce_small, compress = "xz", overwrite = TRUE)
