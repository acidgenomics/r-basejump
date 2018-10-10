#' @inherit Gene2Symbol-class
#'
#' @note
#' - This function will make any duplicated symbols unique by applying
#'   [base::make.unique()], which will add ".1" to the end of the gene name.
#' - No attempt is made to arrange the rows by gene identifier.
#'
#' @name gene2symbol
#' @family Identifier Mapping Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' # SummarizedExperiment ====
#' data(rse_small)
#' x <- gene2symbol(rse_small)
#' print(x)
NULL



#' @rdname gene2symbol
#' @usage NULL
#' @export
Gene2Symbol <- function(object, ...) {
    gene2symbol(object, ...)
}



.gene2symbol.data.frame <-  # nolint
    function(object) {
        assert_has_rows(object)

        # Update legacy column names, if necessary.
        colnames(object) <- sub(
            pattern = "^ensgene$",
            replacement = "geneID",
            x = colnames(object)
        )
        colnames(object) <- sub(
            pattern = "^symbol$",
            replacement = "geneName",
            x = colnames(object)
        )

        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(object))) {
            stop(paste0(
                "Object does not contain gene-to-symbol mappings.\n",
                "Column names: ", toString(colnames(object))
            ), call. = FALSE)
        }

        object %>%
            select(!!!syms(cols)) %>%
            # This is needed for processing GFF files.
            unique() %>%
            mutate_all(as.character) %>%
            mutate(!!sym("geneName") := make.unique(!!sym("geneName"))) %>%
            as("DataFrame") %>%
            new(Class = "Gene2Symbol", .)
    }



.gene2symbol.DataFrame <-  # nolint
    function(object) {
        gene2symbol(as(object, "data.frame"))
    }



.gene2symbol.GRanges <-  # nolint
    function(object) {
        gene2symbol(as(object, "DataFrame"))
    }



.gene2symbol.SE <-  # nolint
    function(object) {
        object %>%
            rowData() %>%
            gene2symbol() %>%
            set_rownames(rownames(object))
    }



#' @rdname gene2symbol
#' @export
setMethod(
    f = "gene2symbol",
    signature = signature("data.frame"),
    definition = .gene2symbol.data.frame
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
