#' Convert Ensembl Transcripts to Genes
#'
#' @name convertTranscriptsToGenes
#' @family Gene Annotation Functions
#' @author Michael Steinbaugh
#'
#' @inherit convertGenesToSymbols
#'
#' @param tx2gene *Optional.* Transcript-to-gene mappings. If `NULL`, will
#'   attempt to download from Ensembl using the desired `organism`,
#'   `genomeBuild`, and `release` arguments.
#'
#' @examples
#' # character ====
#' transcripts <- c(
#'     "ENSMUST00000000001",
#'     "ENSMUST00000000003",
#'     "ENSMUST00000114041"
#' )
#' convertTranscriptsToGenes(transcripts)
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
        tx2gene = NULL,
        organism = NULL,
        genomeBuild = NULL,
        release = NULL
    ) {
        # Passthrough: genomeBuild, release
        assert_is_character(object)
        assert_all_are_non_missing_nor_empty_character(object)
        assert_has_no_duplicates(object)
        assert_is_any_of(tx2gene, c("data.frame", "NULL"))
        assertIsAStringOrNULL(organism)
        assertIsAnImplicitIntegerOrNULL(release)

        # If no tx2gene is provided, fall back to using Ensembl annotations
        if (!is.data.frame(tx2gene)) {
            # Generate tx2gene from Ensembl
            inform("Obtaining transcript-to-gene mappings from Ensembl")
            if (is.null(organism)) {
                organism <- detectOrganism(object, unique = TRUE)
            } else if (is_a_string(organism)) {
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

        tx2gene <- tx2gene %>%
            .[object, , drop = FALSE] %>%
            .[!is.na(.[["geneID"]]), , drop = FALSE]

        geneID <- tx2gene[["geneID"]]
        names(geneID) <- tx2gene[["txID"]]

        if (!all(object %in% names(geneID))) {
            abort(paste(
                "Unmatched transcripts present.",
                "Try using a GFF file instead."
            ))
        }

        assert_is_character(geneID)
        assert_has_names(geneID)

        geneID[object]
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
