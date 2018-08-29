#' Gene to Symbol Mappings
#'
#' @note This function will make any duplicated symbols unique by applying
#' [base::make.unique()], which will add ".1" to the end of the gene name.
#'
#' @name gene2symbol
#' @family Data Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
#'
#' @param unique `boolean`. Ensure that all gene symbols are unique. Uses
#'   [base::make.unique()] internally.
#'
#' @return `data.frame` containing gene identifier and gene name (aka symbol)
#'   mappings.
#'
#' @examples
#' # SummarizedExperiment ====
#' gene2symbol(rse_bcb) %>% head()
NULL



.makeGeneNamesUnique <- function(data) {
    assert_is_subset(
        x = c("geneID", "geneName"),
        y = colnames(data)
    )
    if (any(duplicated(data[["geneName"]]))) {
        x <- data[["geneName"]]
        n <- length(unique(x[duplicated(x)]))
        message(paste(
            "Sanitizing", n, "duplicated symbols using `make.unique()`"
        ))
        data[["geneName"]] <- make.unique(data[["geneName"]])
    }
    data
}



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("SummarizedExperiment"),
    function(
        object,
        unique = TRUE
    ) {
        validObject(object)
        assert_is_a_bool(unique)

        data <- rowData(object)
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(data))) {
            warning("Object does not contain gene-to-symbol mappings")
            return(NULL)
        }
        assert_is_non_empty(data)

        data <- data %>%
            as.data.frame() %>%
            select(!!!syms(cols)) %>%
            mutate_all(as.character) %>%
            set_rownames(rownames(object))

        # Ensure gene names (symbols) are unique, if desired.
        # This is recommended by default.
        if (isTRUE(unique)) {
            data <- .makeGeneNamesUnique(data)
        }

        assertIsGene2symbol(data)
        data
    }
)
