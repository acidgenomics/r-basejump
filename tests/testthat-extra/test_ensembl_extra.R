context("Ensembl annotations")

mat <- .ucscMatrix
keep <- as.logical(mat[, "ensembldb"])
mat <- mat[keep, , drop = FALSE]
rm(keep)

with_parameters_test_that(
    "UCSC genome build remaps", {
        object <- makeGRangesFromEnsembl(
            organism = organism,
            genomeBuild = genomeBuild
        )
        expect_s4_class(object, "GRanges")
    },
    organism = mat[, "organism"],
    genomeBuild = mat[, "ucsc"]
)

rm(mat)
