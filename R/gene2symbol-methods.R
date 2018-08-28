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
#' @return `data.frame` containing gene identifier and gene name (aka symbol)
#'   mappings.
#'
#' @examples
#' # SummarizedExperiment ====
#' gene2symbol(rse_bcb) %>% head()
NULL



#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("SummarizedExperiment"),
    function(object) {
        validObject(object)
        data <- rowData(object)
        cols <- c("geneID", "geneName")
        if (!all(cols %in% colnames(data))) {
            warning("Object does not contain gene-to-symbol mappings")
            return(NULL)
        }
        # Inform the user if there are duplicate symbols.
        if (any(duplicated(data[["geneName"]]))) {
            x <- data[["geneName"]]
            n <- length(unique(x[duplicated(x)]))
            message(paste(
                "Sanitizing", n, "duplicated symbols using `make.unique()`"
            ))
        }
        data <- data %>%
            as("tbl_df") %>%
            select(!!!syms(cols)) %>%
            mutate_all(as.character) %>%
            mutate(geneName = make.unique(!!sym("geneName"))) %>%
            as.data.frame() %>%
            # Note that we're matching the object rownames here.
            set_rownames(rownames(object))
        assertIsGene2symbol(data)
        data
    }
)
