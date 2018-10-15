#' @inherit Gene2Symbol-class
#'
#' @note
#' - This function will make any duplicated symbols unique by applying
#'   [base::make.unique()], which will add ".1" to the end of the gene name.
#' - No attempt is made to arrange the rows by gene identifier.
#'
#' @name Gene2Symbol
#' @family Identifier Mapping Functions
#' @export
#'
#' @inheritParams general
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' data(rse_small)
#' x <- Gene2Symbol(rse_small)
#' print(x)
NULL



.Gene2Symbol.data.frame <-  # nolint
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

        out <- object %>%
            select(!!!syms(cols)) %>%
            # This is needed for processing GFF files.
            unique() %>%
            mutate_all(as.character) %>%
            mutate(!!sym("geneName") := make.unique(!!sym("geneName"))) %>%
            as("DataFrame") %>%
             new(Class = "Gene2Symbol", .)
        metadata(out) <- .prototypeMetadata
        out
    }



.Gene2Symbol.DataFrame <-  # nolint
    function(object) {
        Gene2Symbol(as(object, "data.frame"))
    }



.Gene2Symbol.GRanges <-  # nolint
    function(object) {
        Gene2Symbol(as(object, "DataFrame"))
    }



.Gene2Symbol.SE <-  # nolint
    function(object) {
        object %>%
            rowData() %>%
            Gene2Symbol() %>%
            set_rownames(rownames(object))
    }



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("data.frame"),
    definition = .Gene2Symbol.data.frame
)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("DataFrame"),
    definition = .Gene2Symbol.DataFrame
)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("GRanges"),
    definition = .Gene2Symbol.GRanges
)



#' @rdname Gene2Symbol
#' @export
setMethod(
    f = "Gene2Symbol",
    signature = signature("SummarizedExperiment"),
    definition = .Gene2Symbol.SE
)
