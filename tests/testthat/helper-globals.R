data(
    df, lfc, mat, rse, sce, sparse, txse,
    package = "acidtest",
    envir = environment()
)

# nolint start
DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
IRanges <- IRanges::IRanges
SummarizedExperiment <- SummarizedExperiment::SummarizedExperiment
# nolint end

organism <- "Homo sapiens"
release <- 87L

options(
    acid.save.dir = ".",
    acid.save.ext = "rds",
    acid.test = TRUE
)
