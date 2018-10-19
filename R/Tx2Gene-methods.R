# FIXME Add documentation on where genome information must be stashed.



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

        # Require genome annotation metadata to be defined.
        metadata <- metadata(object)
        mcols <- c("organism", "build", "release")
        if (!all(mcols %in% names(metadata))) {
            warning(paste0(
                "Object does not contain genome information.\n",
                "Requires: ", toString(mcols)
            ))
        }
        metadata <- metadata[mcols]
        metadata <- c(.prototypeMetadata, metadata)

        # Check for required columns.
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            warning(paste0(
                "Object does not contain transcript-to-gene mappings.\n",
                "Requires: ", toString(cols)
            ))
        }

        data <- object %>%
            # Perform this first, otherwise can get a non atomic error due to
            # GRanges to DataFrame coercion containing "X" ranges column.
            .[, cols, drop = FALSE] %>%
            as_tibble(rownames = NULL) %>%
            # This step is needed for handling raw GFF annotations.
            unique() %>%
            mutate_all(as.character) %>%
            as("DataFrame")

        metadata(data) <- metadata
        new(Class = "Tx2Gene", data)
    }



Tx2Gene.GRanges <-  # nolint
    function(object) {
        data <- as(object, "DataFrame")
        metadata(data) <- metadata(object)
        Tx2Gene(data)
    }



Tx2Gene.SummarizedExperiment <-  # nolint
    function(object) {
        validObject(object)
        rownames <- rownames(object)
        if (is(object, "RangedSummarizedExperiment")) {
            data <- rowRanges(object)
        } else {
            data <- rowData(object)
        }
        out <- Tx2Gene(data)
        rownames(out) <- rownames
        out
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
