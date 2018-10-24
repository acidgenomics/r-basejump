#' @inherit Tx2Gene-class
#'
#' @note No attempt is made to arrange the rows by transcript identifier.
#'
#' @name Tx2Gene
#'
#' @inheritParams general
#'
#' @return `Tx2Gene`.
#'
#' @examples
#' ## SummarizedExperiment ====
#' data(tx_se_small)
#' x <- Tx2Gene(tx_se_small)
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
            as("tbl_df") %>%
            mutate_all(as.character) %>%
            as("DataFrame")

        metadata(data) <- .genomeMetadata(object)
        new(Class = "Tx2Gene", data)
    }



Tx2Gene.GRanges <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        # This step is needed for handling raw GFF annotations.
        data <- unique(data)
        metadata(data) <- metadata(object)
        Tx2Gene(data)
    }



Tx2Gene.SummarizedExperiment <-  # nolint
    function(object) {
        object <- as.SummarizedExperiment(object)
        do.call(
            what = Tx2Gene,
            args = list(
                object = rowData(object)
            )
        )
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("DataFrame"),
    definition = Tx2Gene.DataFrame
)



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("GRanges"),
    definition = Tx2Gene.GRanges
)



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = Tx2Gene.SummarizedExperiment
)
