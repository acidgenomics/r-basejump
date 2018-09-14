# TODO Consider collapsing a matrix to unique rows.
# TODO Add SummarizedExperiment method support.



#' Convert Ensembl Transcripts to Genes
#'
#' @name convertTranscriptsToGenes
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @include makeGRanges.R
#' @inherit convertGenesToSymbols
#'
#' @param tx2gene `data.frame` or `NULL`. Transcript-to-gene mappings. If set
#'   `NULL`, the function will attempt to download the mappings from Ensembl
#'   automatically.
#' @param ... Passthrough to [makeTx2geneFromEnsembl()].
#'
#' @examples
#' # character ====
#' x <- c("ENSMUST00000000001", "ENSMUST00000000003", "ENSMUST00000114041")
#' y <- convertTranscriptsToGenes(x)
#' print(y)
#'
#' # matrix ====
#' mat <- matrix(
#'     data = seq(1L:6L),
#'     byrow = TRUE,
#'     nrow = 3L,
#'     ncol = 2L,
#'     dimnames = list(
#'         c("ENSMUST00000000001", "ENSMUST00000000003", "ENSMUST00000114041"),
#'         c("sample1", "sample2")
#'     )
#' )
#' print(mat)
#' mat <- convertTranscriptsToGenes(mat)
#' print(mat)
#' rownames(mat)
NULL



.convertTranscriptsToGenes.character <-  # nolint
function(
    # Setting the formals below.
    object,
    tx2gene = NULL,
    organism = NULL
) {
    assert_all_are_non_missing_nor_empty_character(object)
    assert_has_no_duplicates(object)
    assert_is_any_of(tx2gene, c("DataFrame", "NULL"))
    assertIsAStringOrNULL(organism)

    # If no tx2gene is provided, fall back to using Ensembl annotations.
    if (is.null(tx2gene)) {
        message("Obtaining transcript-to-gene mappings from Ensembl")
        if (is.null(organism)) {
            organism <- detectOrganism(object, unique = TRUE)
        }
        assert_is_a_string(organism)
        message(paste(organism, "genes detected"))
        tx2gene <- do.call(
            what = makeTx2geneFromEnsembl,
            args = matchArgsToDoCall(
                args = list(organism = organism),
                removeFormals = c("object", "tx2gene"),
                n = 2L
            )
        )
    }
    assertIsTx2gene(tx2gene)

    missing <- setdiff(object, tx2gene[["transcriptID"]])
    if (length(missing)) {
        stop(paste("Failed to match transcripts:", toString(missing)))
    }

    tx2gene <- tx2gene[
        match(x = object, table = tx2gene[["transcriptID"]]),
        ,
        drop = FALSE
        ]

    return <- tx2gene[["geneID"]]
    names(return) <- tx2gene[["transcriptID"]]
    return
}

# Set the formals.
f1 <- formals(.convertTranscriptsToGenes.character)
f2 <- formals(makeGRangesFromEnsembl)
f2 <- f2[setdiff(
    x = names(f2),
    y = c(names(f1), "format", "metadata")
)]
f <- c(f1, f2)
formals(.convertTranscriptsToGenes.character) <- f



# Consider aggregating the matrix to gene level instead.
.convertTranscriptsToGenes.matrix <-  # nolint
function(
    # Setting the formals below.
) {
    rownames <- rownames(object)
    rownames <- do.call(
        what = convertTranscriptsToGenes,
        args = matchArgsToDoCall(
            args = list(object = rownames),
            n = 2L
        )
    )
    rownames(object) <- rownames
    object
}

# Set the formals.
f <- formals(.convertTranscriptsToGenes.character)
formals(.convertTranscriptsToGenes.matrix) <- f



#' @rdname convertTranscriptsToGenes
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("character"),
    .convertTranscriptsToGenes.character
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("matrix"),
    .convertTranscriptsToGenes.matrix
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("data.frame"),
    definition = getMethod("convertTranscriptsToGenes", "matrix")
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("DataFrame"),
    definition = getMethod("convertTranscriptsToGenes", "data.frame")
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("sparseMatrix"),
    definition = getMethod("convertTranscriptsToGenes", "matrix")
)
