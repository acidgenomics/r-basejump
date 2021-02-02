#' @name convertTranscriptsToGenes
#' @inherit AcidGenerics::convertTranscriptsToGenes
#'
#' @note For objects containing a count matrix, the object rows will be
#'   collapsed to gene level using `aggregateRows`. This applies to our
#'   `SummarizedExperiment` method.
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param aggregate `logical(1)`.
#'   For objects supporting [`dim()`][base::dim], aggregate counts to gene level
#'   and collapse the matrix.
#' @param ... Additional arguments.
#'
#' @return
#' - `character`: `factor`.
#'     Genes in the values, transcripts in the names.
#' - `matrix`, `Matrix`, `SummarizedExperiment`:
#'     Object containing counts collapsed to gene level by default
#'     (see `aggregate` argument).
#'
#' @seealso [aggregateRows()].
#'
#' @examples
#' data(SummarizedExperiment_transcripts, package = "AcidTest")
#' txse <- SummarizedExperiment_transcripts
#' object <- txse
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



## Updated 2021-02-02.
`convertTranscriptsToGenes,character` <-  # nolint
    function(object, tx2gene) {
        assert(
            isCharacter(object),
            hasNoDuplicates(object),
            is(tx2gene, "Tx2Gene")
        )
        ## Arrange the tx2gene to match the input.
        cols <- c("txId", "geneId")
        if (!identical(cols, colnames(tx2gene))) {
            colnames(tx2gene) <- cols
        }
        validObject(tx2gene)
        missing <- setdiff(object, tx2gene[["txId"]])
        if (length(missing) > 0L) {
            stop(sprintf(
                "Failed to match transcripts: %s.",
                toString(missing, width = 100L)
            ))
        }
        tx2gene <- tx2gene[
            match(x = object, table = tx2gene[["txId"]]),
            ,
            drop = FALSE
        ]
        out <- as.factor(tx2gene[["geneId"]])
        names(out) <- tx2gene[["txId"]]
        out
    }



#' @rdname convertTranscriptsToGenes
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("character"),
    definition = `convertTranscriptsToGenes,character`
)



## Consider aggregating the matrix to gene level instead.
## Updated 2020-01-30.
`convertTranscriptsToGenes,matrix` <-  # nolint
    function(object, tx2gene, aggregate = TRUE) {
        assert(isFlag(aggregate))
        t2g <- do.call(
            what = convertTranscriptsToGenes,
            args = list(
                object = rownames(object),
                tx2gene = tx2gene
            )
        )
        if (isTRUE(aggregate)) {
            aggregateRows(object, by = t2g)
        } else {
            rownames(object) <- as.character(t2g)
            object
        }
    }



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("matrix"),
    definition = `convertTranscriptsToGenes,matrix`
)



## Updated 2020-01-30.
`convertTranscriptsToGenes,Matrix` <-  # nolint
    `convertTranscriptsToGenes,matrix`

#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("Matrix"),
    definition = `convertTranscriptsToGenes,Matrix`
)



## Consider returning RSE here in a future update.
## Need to add code that handles rowRanges.
## Updated 2019-07-22.
`convertTranscriptsToGenes,SummarizedExperiment` <-  # nolint
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
        assert(identical(
            x = colSums(counts(object)),
            y = colSums(counts(se))
        ))
        se
    }



#' @rdname convertTranscriptsToGenes
#' @export
setMethod(
    f = "convertTranscriptsToGenes",
    signature = signature("SummarizedExperiment"),
    definition = `convertTranscriptsToGenes,SummarizedExperiment`
)
