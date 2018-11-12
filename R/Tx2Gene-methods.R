#' @name Tx2Gene
#' @inherit Tx2Gene-class
#' @inheritParams params
#' @note No attempt is made to arrange the rows by transcript identifier.
#' @examples
#' ## SummarizedExperiment ====
#' data(tx_se)
#' x <- Tx2Gene(tx_se)
#' print(x)
NULL



Tx2Gene.DataFrame <-  # nolint
    function(object) {
        assert_has_rows(object)

        # Check for required columns.
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain transcript-to-gene mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object %>%
            .[, cols, drop = FALSE] %>%
            # This will handle rownames automatically, even if unset.
            as("tbl_df") %>%
            mutate_all(as.character) %>%
            as("DataFrame")

        metadata(data) <- .genomeMetadata(object)
        new(Class = "Tx2Gene", data)
    }



#' @rdname Tx2Gene
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



#' @rdname Tx2Gene
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
        do.call(
            what = Tx2Gene,
            args = list(object = data)
        )
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = Tx2Gene.SummarizedExperiment
)
