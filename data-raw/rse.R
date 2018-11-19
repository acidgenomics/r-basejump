# Gene-level RangedSummarizedExperiment example
# 2018-11-19

library(pryr)
library(DESeq2)
library(tidyverse)

# Restrict to 1 MB.
# Use `pryr::object_size()` instead of `utils::object.size()`.
limit <- structure(2e6, class = "object_size")

organism <- "Homo sapiens"
release <- 92L

# Generate example DESeqDataSet using DESeq2.
# Note that we're using simulated counts here.
dds <- makeExampleDESeqDataSet(n = 500L, m = 12L, betaSD = 1L)
object_size(dds)
stopifnot(object_size(dds) < limit)
stopifnot(validObject(dds))

# Coerce to RangedSummarizedExperiment.
# Need to change rows to actual gene identifiers here, and slot colData.
rse <- as(dds, "RangedSummarizedExperiment")
object_size(rse)
stopifnot(object_size(rse) < limit)

# Pad the dimnames so they sort correctly.
rownames(rse) <- rownames(rse) %>%
    str_replace("gene", "") %>%
    str_pad(width = 3L, side = "left", pad = "0") %>%
    paste0("gene", .)
colnames(rse) <- colnames(rse) %>%
    str_replace("sample", "") %>%
    str_pad(width = 2L, side = "left", pad = "0") %>%
    paste0("sample", .)

# Column data.
# Note that `sampleName` column is generated for `sampleData()` return.
colData(rse) <- DataFrame(
    genotype = factor(
        rep(c("wildtype", "knockout"), times = ncol(rse) / 2L),
        levels = c("wildtype", "knockout")
    ),
    treatment = factor(
        rep(c("control", "treated"), each = ncol(rse) / 2L),
        levels = c("control", "treated")
    ),
    row.names = colnames(rse)
)

# Row data.
# Include real `geneID`, `geneName` columns to test mapping functions.
rowRanges <- makeGRangesFromEnsembl(organism, release = release)
# Subset to match the number of rows in the example.
rowRanges <- rowRanges[seq_len(nrow(rse))]
# Note that we're keeping the original rownames from dds_small, and they won't
# match the `geneID` column in rowRanges. This is intentional, for unit testing.
names(rowRanges) <- rownames(rse)
# Select only `geneID` and `geneName` columns, to keep example small.
# If factor, make sure we use `droplevels()` here to keep object small.
mcols(rowRanges) <- mcols(rowRanges) %>%
    as("tbl_df") %>%
    select(
        rowname,
        geneID,
        geneName,
        geneBiotype,
        broadClass,
        entrezID
    ) %>%
    mutate_if(is.factor, as.character) %>%
    as("DataFrame")
rowRanges(rse) <- rowRanges

# Metadata.
# Stash the date.
metadata(rse)[["date"]] <- Sys.Date()
# Define the interesting groups.
interestingGroups(rse) <- c("genotype", "treatment")

# Size check.
vapply(
    X = coerceS4ToList(rse),
    FUN = object_size,
    FUN.VALUE = numeric(1L)
)
object_size(rse)
stopifnot(object_size(rse) < limit)
stopifnot(validObject(rse))

usethis::use_data(rse, compress = "xz", overwrite = TRUE)
