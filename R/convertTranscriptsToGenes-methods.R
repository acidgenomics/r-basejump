#' Convert Ensembl Transcripts to Genes
#'
#' @note For objects containing a count matrix, the object rows will be
#'   collapsed to gene level using [aggregateRows()]. This applies to our
#'   `SummarizedExperiment` method.
#'
#' @name convertTranscriptsToGenes
#' @family Identifier Mapping Functions
#' @family Transcript-Level Functions
#' @include makeTx2Gene.R
#' @inherit convertGenesToSymbols
#'
#' @inheritParams general
#' @param aggregate `boolean`. For objects supporting [base::dim()], aggregate
#'   counts to gene level and collapse the matrix.
#' @param ... Passthrough to [makeTx2GeneFromEnsembl()].
#'
#' @return
#' - `character`: `factor`. Genes in the values, transcripts in the names.
#' - `matrix`, `sparseMatrix`, `SummarizedExperiment`: Object containing counts
#'   collapsed to gene level by default (see `aggregate` argument).
#'
#' @seealso [aggregateRows()].
#'
#' @examples
#' data(tx_se_small)
#' object <- tx_se_small
#'
#' t2g <- Tx2Gene(object)
#' print(t2g)
#' transcripts <- rownames(object)
#' print(transcripts)
#'
#' ## character ====
#' ## Returns as factor.
#' x <- convertTranscriptsToGenes(transcripts, tx2gene = t2g)
#' print(x)
#' str(x)
#'
#' ## matrix ====
#' ## Note that transcript IDs currently must be in the rows.
#' counts <- counts(object)
#' print(counts)
#' ## Aggregate to gene level.
#' x <- convertTranscriptsToGenes(counts, tx2gene = t2g, aggregate = TRUE)
#' print(x)
#' colSums(x)
#' ## Simply map to rownames.
#' x <- convertTranscriptsToGenes(counts, tx2gene = t2g, aggregate = FALSE)
#' print(x)
#' colSums(x)
#'
#' ## SummarizedExperiment ====
#' x <- convertTranscriptsToGenes(object)
#' print(x)
NULL



convertTranscriptsToGenes.character <-  # nolint
    function(object, tx2gene) {
        assert_all_are_non_missing_nor_empty_character(object)
        assert_has_no_duplicates(object)
        assert_is_all_of(tx2gene, "Tx2Gene")
        validObject(tx2gene)

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

        out <- as.factor(tx2gene[["geneID"]])
        names(out) <- tx2gene[["transcriptID"]]
        out
    }



# Consider aggregating the matrix to gene level instead.
convertTranscriptsToGenes.matrix <-  # nolint
    function(object, tx2gene, aggregate = TRUE) {
        assert_is_a_bool(aggregate)
        t2g <- do.call(
            what = convertTranscriptsToGenes,
            args = list(
                object = rownames(object),
                tx2gene = tx2gene
            )
        )
        if (isTRUE(aggregate)) {
            aggregateRows(object, groupings = t2g)
        } else {
            rownames(object) <- as.character(t2g)
            object
        }
    }



# Consider returning RSE here in a future update.
# Need to add code that handles rowRanges.
convertTranscriptsToGenes.SummarizedExperiment <-  # nolint
    function(object) {
        counts <- counts(object)
        t2g <- Tx2Gene(object)
        counts <- convertTranscriptsToGenes(
            object = counts,
            tx2gene = t2g,
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
    definition = convertTranscriptsToGenes.character
)



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("matrix"),
    definition = convertTranscriptsToGenes.matrix
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
    definition = convertTranscriptsToGenes.SummarizedExperiment
)
