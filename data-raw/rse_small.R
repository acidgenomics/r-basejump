# Gene-level RangedSummarizedExperiment example
# Last updated 2018-10-08

library(DESeq2)
library(tidyverse)

organism <- "Homo sapiens"
release <- 92L

# Restrict to 1 MB per file.
limit <- structure(1e6, class = "object_size")

# Generate example DESeqDataSet using DESeq2.
# Note that we're using simulated counts here.
dds <- makeExampleDESeqDataSet(n = 50L, m = 4L)
stopifnot(object.size(dds) < limit)
stopifnot(validObject(dds))

# Coerce to RangedSummarizedExperiment.
# Need to change rows to actual gene identifiers here, and slot colData.
rse <- as(dds, "RangedSummarizedExperiment")
stopifnot(object.size(rse) < limit)

# Column data.
# Note that `sampleName` column is generated for `sampleData()` return.
colData(rse) <- DataFrame(
    genotype = factor(
        rep(c("wildtype", "knockout"), times = 2L),
        levels = c("wildtype", "knockout")
    ),
    treatment = factor(
        rep(c("control", "treated"), each = 2L),
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
    FUN = object.size,
    FUN.VALUE = numeric(1L)
)
stopifnot(object.size(rse) < limit)
stopifnot(validObject(rse))

rse_small <- rse
devtools::use_data(rse_small, compress = "xz", overwrite = TRUE)
