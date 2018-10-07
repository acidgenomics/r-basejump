#' Gene-to-Symbol Mappings
#'
#' @note This function will make any duplicated symbols unique by applying
#' [base::make.unique()], which will add ".1" to the end of the gene name.
#'
#' @name gene2symbol
#' @family S4 Generators
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- gene2symbol(rse_small)
#' print(x)
NULL



#' @rdname gene2symbol
#' @export
Gene2Symbol <- function(object, ...) {
    gene2symbol(object, ...)
}



.gene2symbol.tbl_df <-  # nolint
    function(object) {
        assert_has_rows(object)
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(
                "Object does not contain gene-to-symbol mappings.",
                call. = FALSE
            )
        }
        object %>%
            select(!!!syms(cols)) %>%
            mutate_all(as.character) %>%
            mutate(!!sym("geneName") := make.unique(!!sym("geneName"))) %>%
            as("DataFrame") %>%
            new(Class = "Gene2Symbol", .)
    }



.gene2symbol.DataFrame <-  # nolint
    function(object) {
        object %>%
            as_tibble(rownames = NULL) %>%
            gene2symbol() %>%
            set_rownames(rownames(object))
    }



.gene2symbol.GRanges <-  # nolint
    function(object) {
        object %>%
            as("DataFrame") %>%
            gene2symbol()
    }



.gene2symbol.SE <-  # nolint
    function(object) {
        object %>%
            rowData() %>%
            set_rownames(rownames(object)) %>%
            gene2symbol()
    }



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("tbl_df"),
    definition = .gene2symbol.tbl_df
)



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("DataFrame"),
    definition = .gene2symbol.DataFrame
)



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("GRanges"),
    definition = .gene2symbol.GRanges
)



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("SummarizedExperiment"),
    definition = .gene2symbol.SE
)
