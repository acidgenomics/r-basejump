#' @inherit Tx2Gene-class
#'
#' @note No attempt is made to arrange the rows by transcript identifier.
#'
#' @name Tx2Gene
#' @family Identifier Mapping Functions
#' @family Transcript-Level Functions
#' @export
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



.Tx2Gene.data.frame <-  # nolint
    function(object) {
        assert_has_rows(object)

        # Update legacy column names, if necessary.
        colnames(object) <- sub(
            pattern = "^enstxp|txID$",
            replacement = "transcriptID",
            x = colnames(object)
        )
        colnames(object) <- sub(
            pattern = "^ensgene$",
            replacement = "geneID",
            x = colnames(object)
        )

        # Check that required columns are present.
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain transcript-to-gene mappings.\n",
                "Column names: ", toString(colnames(object))
            ), call. = FALSE)
        }

        out <- object %>%
            select(!!!syms(c("transcriptID", "geneID"))) %>%
            # This is needed for processing GFF files.
            unique() %>%
            mutate_all(as.character) %>%
            as("DataFrame") %>%
            new(Class = "Tx2Gene", .)
        metadata(out) <- .prototypeMetadata
        out
    }



.Tx2Gene.DataFrame <-  # nolint
    function(object) {
        Tx2Gene(as(object, "data.frame"))
    }



.Tx2Gene.GRanges <-  # nolint
    function(object) {
        Tx2Gene(as(object, "DataFrame"))
    }



.Tx2Gene.SE <-  # nolint
    function(object) {
        validObject(object)
        object %>%
            rowData() %>%
            Tx2Gene() %>%
            set_rownames(rownames(object))
    }



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("data.frame"),
    definition = .Tx2Gene.data.frame
)



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("DataFrame"),
    definition = .Tx2Gene.DataFrame
)



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("GRanges"),
    definition = .Tx2Gene.GRanges
)



#' @rdname Tx2Gene
#' @export
setMethod(
    f = "Tx2Gene",
    signature = signature("SummarizedExperiment"),
    definition = .Tx2Gene.SE
)
