data(
    DataFrame,
    GRanges,
    matrix,
    matrix_lfc,
    RangedSummarizedExperiment,
    SingleCellExperiment,
    sparseMatrix,
    SummarizedExperiment_transcripts,
    package = "acidtest",
    envir = environment()
)

df <- DataFrame
gr <- GRanges
lfc <- matrix_lfc
mat <- matrix
rse <- RangedSummarizedExperiment
sce <- SingleCellExperiment
sparse <- sparseMatrix
txse <- SummarizedExperiment_transcripts

## nolint start
DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
IRanges <- IRanges::IRanges
SummarizedExperiment <- SummarizedExperiment::SummarizedExperiment
cause <- goalie::cause
group_vars <- dplyr::group_vars
hasInternet <- goalie::hasInternet
str_pad <- stringr::str_pad
## nolint end

options(
    acid.save.dir = ".",
    acid.save.ext = "rds",
    acid.test = TRUE
)

## FIXME Warn if test URL doesn't exist.
