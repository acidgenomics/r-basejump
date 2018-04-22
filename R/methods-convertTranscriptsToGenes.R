#' Convert Ensembl Transcripts to Genes
#'
#' @name convertTranscriptsToGenes
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inherit convertGenesToSymbols
#'
#' @param tx2gene *Optional.* Transcript-to-gene mappings. If missing, the
#'   function will attempt to download mappings from Ensembl according to the
#'   `organism`, `genomeBuild`, and `release` parameters.
#'
#' @examples
#' # character ====
#' x <- c("ENSMUST00000000001", "ENSMUST00000000003", "ENSMUST00000114041")
#' convertTranscriptsToGenes(x)
#'
#' # matrix ====
#' mat <- matrix(
#'     data = seq(1L:6L),
#'     byrow = TRUE,
#'     nrow = 3L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("ENSMUST00000000001", "ENSMUST00000000003", "ENSMUST00000114041"),
#'         c("sample_1", "sample_2")
#'     )
#' )
#' print(mat)
#' mat <- convertTranscriptsToGenes(mat)
#' print(mat)
#' rownames(mat)
NULL



# Methods ======================================================================
#' @rdname convertTranscriptsToGenes
setMethod(
    "convertTranscriptsToGenes",
    signature("character"),
    function(
        object,
        tx2gene,
        organism,
        genomeBuild = NULL,
        release = NULL
    ) {
        # Passthrough: genomeBuild, release
        assert_is_character(object)
        assert_all_are_non_missing_nor_empty_character(object)
        assert_has_no_duplicates(object)

        # If no tx2gene is provided, fall back to using Ensembl annotations
        if (missing(tx2gene) || is.null(tx2gene)) {
            message("Obtaining transcript-to-gene mappings from Ensembl")
            if (missing(organism) || is.null(organism)) {
                organism <- detectOrganism(object, unique = TRUE)
            } else {
                organism <- detectOrganism(organism)
            }
            assert_is_a_string(organism)
            tx2gene <- makeTx2geneFromEnsembl(
                organism = organism,
                genomeBuild = genomeBuild,
                release = release
            )
        } else {
            assertIsTx2gene(tx2gene)
        }

        if (!all(object %in% tx2gene[["txID"]])) {
            stop(paste(
                "Unmatched transcripts present.",
                "Try using a GFF file instead."
            ))
        }

        tx2gene <- tx2gene[match(object, tx2gene[["txID"]]), , drop = FALSE]
        genes <- tx2gene[["geneID"]]
        names(genes) <- tx2gene[["txID"]]
        genes
    }
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("matrix"),
    function(object, ...) {
        rownames <- convertTranscriptsToGenes(rownames(object), ...)
        rownames(object) <- rownames
        object
    }
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("data.frame"),
    getMethod("convertTranscriptsToGenes", "matrix")
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("DataFrame"),
    getMethod("convertTranscriptsToGenes", "data.frame")
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("dgCMatrix"),
    getMethod("convertTranscriptsToGenes", "matrix")
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    "convertTranscriptsToGenes",
    signature("dgTMatrix"),
    getMethod("convertTranscriptsToGenes", "matrix")
)
