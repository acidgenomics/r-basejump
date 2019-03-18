context("Extra : Ensembl annotations")

mat <- .ucscMatrix
keep <- as.logical(mat[, "ensembldb"])
mat <- mat[keep, , drop = FALSE]
rm(keep)

# Note that we're running at transcript level here to check the gene merge code.
#
# Potential issues:
#
# - *Caenorhabditis elegans*
#     - Invalid transcript IDs.
# - *Canis familiaris*
#     - Using the full *Canis lupus familiaris* won't match.
# - *Saccharomyces cerevisiae*
#     - Invalid gene IDs.
#     - Iinvalid transcript IDs.
#     - No gene names.

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
