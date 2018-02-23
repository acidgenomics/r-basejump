#' Gene Annotations
#'
#' @rdname genes
#' @name genes
#'
#' @importFrom GenomicFeatures genes
#'
#' @section Broad Class Definitions:
#' A `broadClass` column is added, which generalizes the gene types into a
#' smaller number of semantically-meaningful groups:
#'
#'   - `coding`
#'   - `noncoding`
#'   - `pseudo`
#'   - `small`
#'   - `decaying`
#'   - `ig` (immunoglobulin)
#'   - `tcr` (T cell receptor)
#'   - `other`

#' @param uniqueSymbol Make gene symbols unique.
#'
#' @return `GRanges`, `data.frame`, or `DataFrame`.
#'
#' @examples
#' # Legacy GRCh37/hg19 genome build support
#' genes("Homo sapiens", genomeBuild = "GRCh37") %>% glimpse()
NULL



# Constructors =================================================================




#' @importFrom dplyr distinct everything group_by left_join mutate rename select
#'   summarize_all ungroup
#' @importFrom rlang !! sym
.genes.dataFrame <- function(x, uniqueSymbol = FALSE) {  # nolint
    assert_is_data.frame(x)
    assert_are_identical(colnames(x), ensembldbGeneCols)
    # FIXME These are duplicated
    x %>%
        rename(
            ensgene = .data[["gene_id"]],
            biotype = .data[["gene_biotype"]],
            entrez = .data[["entrezid"]]
        ) %>%
        # Use `symbol` column instead of duplicate `gene_name` column
        mutate(gene_name = NULL) %>%
        camel(colnames = TRUE, rownames = FALSE) %>%
        .broadClass() %>%
        # Ensure rows are sorted by gene ID
        arrange(!!sym("ensgene")) %>%
        set_rownames(.[["ensgene"]])
}



# Methods ======================================================================
#' @rdname genes
#' @export
setMethod(
    "genes",
    signature("character"),
    function(
        x,
        genomeBuild = NULL,
        release = NULL,
        uniqueSymbol = FALSE,
        return = "data.frame") {
        data <- ensemblAnnotations(
            organism = x,
            format = "genes",
            genomeBuild = genomeBuild,
            release = release,
            return = return)
        genes(data, uniqueSymbol = uniqueSymbol)
    }
)



#' @rdname genes
#' @export
setMethod(
    "genes",
    signature("data.frame"),
    .genes.dataFrame)



