#' @rdname Tx2Gene-class
#' @name Tx2Gene
#' @note No attempt is made to arrange the rows by transcript identifier.
#' @inheritParams params
#' @seealso `makeTx2Gene()`.
#' @examples
#' ## SummarizedExperiment ====
#' data(txse, package = "acidtest")
#' x <- Tx2Gene(txse)
#' print(x)
NULL



Tx2Gene.DataFrame <-  # nolint
    function(object) {
        assert(hasRows(object))

        # Check for required columns.
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain transcript-to-gene mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- DataFrame(
            transcriptID = as.character(decode(object[["transcriptID"]])),
            geneID = as.character(decode(object[["geneID"]])),
            row.names = rownames(object)
        )

        metadata(data) <- .slotGenomeMetadata(object)
        new(Class = "Tx2Gene", data)
    }



#' @rdname Tx2Gene-class
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("DataFrame"),
    definition = Tx2Gene.DataFrame
)



Tx2Gene.GRanges <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        # This step is needed for handling raw GFF annotations.
        data <- unique(data)
        metadata(data) <- metadata(object)
        Tx2Gene(data)
    }



#' @rdname Tx2Gene-class
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("GRanges"),
    definition = Tx2Gene.GRanges
)



Tx2Gene.SummarizedExperiment <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        do.call(what = Tx2Gene, args = list(object = data))
    }



#' @rdname Tx2Gene-class
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = Tx2Gene.SummarizedExperiment
)
