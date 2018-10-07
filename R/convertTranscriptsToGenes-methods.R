# FIXME Speed up the working example.
# FIXME Improve working example to show `aggregate = TRUE` more clearly.



#' Convert Ensembl Transcripts to Genes
#'
#' @note
#' For objects containing a counts matrix, the object rows will be collapsed to
#' gene level using [aggregateRows()]. This applies to our
#' `SummarizedExperiment` method.
#'
#' @name convertTranscriptsToGenes
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @include makeTx2Gene.R
#' @inherit convertGenesToSymbols
#'
#' @param tx2gene `Tx2Gene` or `NULL`. Transcript-to-gene mappings. If set
#'   `NULL`, the function will attempt to download the mappings from Ensembl
#'   automatically.
#' @param aggregate `boolean`. For objects supporting [dim()], aggregate counts
#'   to gene level and collapse the matrix.
#' @param ... Passthrough to [makeTx2geneFromEnsembl()].
#'
#' @return
#' - `character`: `factor`. Genes in the values, transcripts in the names.
#' - `matrix`, `sparseMatrix`, `SummarizedExperiment`: Object containing counts
#'   collapsed to gene level by default (see `aggregate` argument).
#'
#' @seealso [aggregateRows()].
#'
#' @examples
#' tx2gene <- tx2gene(tx_se_small)
#' print(tx2gene)
#' transcriptIDs <- rownames(tx_se_small)
#' print(transcriptIDs)
#' stopifnot(all(transcriptIDs %in% rownames(tx2gene)))
#'
#' # character ====
#' # Returns as factor.
#' x <- convertTranscriptsToGenes(transcriptIDs, tx2gene = tx2gene)
#' print(x)
#' str(x)
#'
#' # matrix ====
#' # Note that transcript IDs currently must be in the rows.
#' counts <- counts(tx_se_small)
#' print(counts)
#' # Aggregate to gene level.
#' x <- convertTranscriptsToGenes(counts, tx2gene = tx2gene, aggregate = TRUE)
#' print(x)
#' colSums(x)
#' # Simply map to rownames.
#' x <- convertTranscriptsToGenes(counts, tx2gene = tx2gene, aggregate = FALSE)
#' print(x)
#' colSums(x)
#'
#' # SummarizedExperiment ====
#' object <- tx_se_small
#' print(object)
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
        assert_is_any_of(tx2gene, c("Tx2Gene", "NULL"))
        assertIsAStringOrNULL(organism)

        # If no tx2gene is provided, fall back to using Ensembl annotations.
        if (is.null(tx2gene)) {
            message("Obtaining transcript-to-gene mappings from Ensembl...")
            if (is.null(organism)) {
                organism <- organism(object)
            }
            assert_is_a_string(organism)
            message(paste(organism, "genes detected."))
            tx2gene <- do.call(
                what = makeTx2geneFromEnsembl,
                args = matchArgsToDoCall(
                    args = list(organism = organism),
                    removeFormals = c("object", "tx2gene")
                )
            )
        }
        assert_is_all_of(tx2gene, "Tx2Gene")

        missing <- setdiff(object, tx2gene[["transcriptID"]])
        if (has_length(missing)) {
            stop(paste(
                "Failed to match transcripts:", toString(missing)
            ), call. = FALSE)
        }

        tx2gene <- tx2gene[
            match(x = object, table = tx2gene[["transcriptID"]]),
            ,
            drop = FALSE
            ]

        return <- as.factor(tx2gene[["geneID"]])
        names(return) <- tx2gene[["transcriptID"]]
        return
    }
f1 <- formals(.convertTranscriptsToGenes.character)
f2 <- formals(makeTx2GeneFromEnsembl)
f2 <- f2[setdiff(names(f2), c(names(f1)))]
f <- c(f1, f2)
formals(.convertTranscriptsToGenes.character) <- f



# Consider aggregating the matrix to gene level instead.
.convertTranscriptsToGenes.matrix <-  # nolint
    function(aggregate = TRUE) {
        assert_is_a_bool(aggregate)
        t2g <- do.call(
            what = convertTranscriptsToGenes,
            args = matchArgsToDoCall(
                args = list(object = rownames(object)),
                removeFormals = "aggregate"
            )
        )
        if (isTRUE(aggregate)) {
            aggregateRows(object, groupings = t2g)
        } else {
            rownames(object) <- as.character(t2g)
            object
        }
    }
f1 <- formals(.convertTranscriptsToGenes.character)
f2 <- formals(.convertTranscriptsToGenes.matrix)
f <- c(f1, f2)
formals(.convertTranscriptsToGenes.matrix) <- f



# Consider returning RSE here in a future update.
# Need to add code that handles rowRanges.
.convertTranscriptsToGenes.SE <-  # nolint
    function(object) {
        tx2gene <- tx2gene(object)
        counts <- counts(object)
        t2g <- as.factor(tx2gene[["geneID"]])
        names(t2g) <- tx2gene[["transcriptID"]]
        counts <- convertTranscriptsToGenes(
            object = counts,
            tx2gene = tx2gene,
            aggregate = TRUE
        )
        se <- SummarizedExperiment(
            assays = list(counts = counts),
            colData = colData(object)
        )
        assert_are_identical(
            x = colSums(counts(object)),
            y = colSums(counts(se))
        )
        se
    }



#' @rdname convertTranscriptsToGenes
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("character"),
    definition = .convertTranscriptsToGenes.character
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("matrix"),
    definition = .convertTranscriptsToGenes.matrix
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("sparseMatrix"),
    definition = getMethod(
        f = "convertTranscriptsToGenes",
        signature = signature("matrix")
    )
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = .convertTranscriptsToGenes.SE
)
