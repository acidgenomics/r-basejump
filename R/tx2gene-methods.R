#' @inherit Tx2Gene-class
#'
#' @note No attempt is made to arrange the rows by transcript identifier.
#'
#' @name tx2gene
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
#' x <- tx2gene(tx_se_small)
#' print(x)
NULL



#' @rdname tx2gene
#' @usage NULL
#' @export
Tx2Gene <- function(object, ...) {
    tx2gene(object, ...)
}



.tx2gene.data.frame <-  # nolint
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

        object %>%
            select(!!!syms(c("transcriptID", "geneID"))) %>%
            # This is needed for processing GFF files.
            unique() %>%
            mutate_all(as.character) %>%
            as("DataFrame") %>%
            new(Class = "Tx2Gene", .)
    }



.tx2gene.DataFrame <-  # nolint
    function(object) {
        tx2gene(as(object, "data.frame"))
    }



.tx2gene.GRanges <-  # nolint
    function(object) {
        tx2gene(as(object, "DataFrame"))
    }



.tx2gene.SE <-  # nolint
    function(object) {
        validObject(object)
        object %>%
            rowData() %>%
            tx2gene() %>%
            set_rownames(rownames(object))
    }



#' @rdname tx2gene
#' @export
setMethod(
    f = "tx2gene",
    signature = signature("data.frame"),
    definition = .tx2gene.data.frame
)



#' @rdname tx2gene
#' @export
setMethod(
    f = "tx2gene",
    signature = signature("DataFrame"),
    definition = .tx2gene.DataFrame
)



#' @rdname tx2gene
#' @export
setMethod(
    f = "tx2gene",
    signature = signature("GRanges"),
    definition = .tx2gene.GRanges
)



#' @rdname tx2gene
#' @export
setMethod(
    f = "tx2gene",
    signature = signature("SummarizedExperiment"),
    definition = .tx2gene.SE
)
