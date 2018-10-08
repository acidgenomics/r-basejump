#' `Tx2Gene` Generator
#'
#' @name tx2gene
#' @family S4 Generators
#' @author Michael Steinbaugh
#' @inherit Tx2Gene-class
#' @export
#'
#' @inheritParams general
#'
#' @return `Tx2Gene`.
#'
#' @examples
#' # SummarizedExperiment ====
#' data(tx_se_small)
#' x <- tx2gene(tx_se_small)
#' print(x)
NULL



.tx2gene.tbl_df <-  # nolint
    function(object) {
        assert_has_rows(object)
        cols <- c("transcriptID", "geneID")
        if (!all(cols %in% colnames(object))) {
            stop(
                "Object does not contain transcript-to-gene mappings.",
                call. = FALSE
            )
        }
        object %>%
            select(!!!syms(c("transcriptID", "geneID"))) %>%
            mutate_all(as.character) %>%
            as("DataFrame") %>%
            new(Class = "Tx2Gene", .)
    }



.tx2gene.DataFrame <-  # nolint
    function(object) {
        object %>%
            as("tbl_df") %>%
            tx2gene()
    }



.tx2gene.GRanges <-  # nolint
    function(object) {
        object %>%
            as("DataFrame") %>%
            tx2gene()
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
    signature = signature("tbl_df"),
    definition = .tx2gene.tbl_df
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
