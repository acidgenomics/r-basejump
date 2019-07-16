data(
    df, gr, lfc, mat, rse, sce, sparse, txse,
    package = "acidtest",
    envir = environment()
)

# nolint start
DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
IRanges <- IRanges::IRanges
SummarizedExperiment <- SummarizedExperiment::SummarizedExperiment
cause <- goalie::cause
group_vars <- dplyr::group_vars
# nolint end

options(
    acid.save.dir = ".",
    acid.save.ext = "rds",
    acid.test = TRUE
)
