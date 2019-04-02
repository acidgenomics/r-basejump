data(
    lfc, mat, rse, sce,
    package = "acidtest",
    envir = environment()
)

# nolint start
DataFrame <- S4Vectors::DataFrame
GRanges <- GenomicRanges::GRanges
IRanges <- IRanges::IRanges
# nolint end

organism <- "Homo sapiens"
release <- 87L

options(
    acid.save.dir = ".",
    acid.save.ext = "rds",
    acid.test = TRUE
)
