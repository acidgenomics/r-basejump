context("Ensembl annotations")

mat <- .ucscMatrix
keep <- as.logical(mat[, "ensembldb"])
mat <- mat[keep, , drop = FALSE]
rm(keep)

# Note that we're running at transcript level here to check the gene merge code.
# Potential issues:
# - *C. elegans*:
#     - Invalid transcript IDs.
# - *S. cerevisiae*:
#     - Invalid gene IDs.
#     - Iinvalid transcript IDs.
#     - No `geneName` in `mcols()`.
with_parameters_test_that(
    "UCSC genome build remaps", {
        object <- makeGRangesFromEnsembl(
            organism = organism,
            genomeBuild = genomeBuild,
            level = "transcripts"
        )
        expect_s4_class(object, "GRanges")
    },
    organism = mat[, "organism"],
    genomeBuild = mat[, "ucsc"]
)

rm(mat)
