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
#' Define Broad Class
#'
#' @author Broad class definitions by Rory Kirchner
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr case_when mutate
#' @importFrom rlang .data
#' @importFrom tibble column_to_rownames rownames_to_column
#'
#' @return Data frame containing `broadClass` column.
.defineBroadClass <- function(object) {
    assert_is_data.frame(object)
    assert_is_subset(c("biotype", "symbol"), colnames(object))
    object %>%
        rownames_to_column() %>%
        mutate(
            broadClass = case_when(
                grepl(
                    x = .data[["symbol"]],
                    # Hsapiens: `MT-`,
                    # Mmusculus: `mt-`
                    # Dmelanogaster: `mt:`
                    pattern = "^mt[\\:\\-]",
                    ignore.case = TRUE
                ) ~ "mito",
                .data[["biotype"]] == "protein_coding" ~ "coding",
                .data[["biotype"]] %in% c(
                    "known_ncrna",
                    "lincRNA",
                    "non_coding"
                ) ~ "noncoding",
                grepl(
                    pattern = "pseudo",
                    x = .data[["biotype"]]
                ) ~ "pseudo",
                .data[["biotype"]] %in% c(
                    "miRNA",
                    "misc_RNA",
                    "ribozyme",
                    "rRNA",
                    "scaRNA",
                    "scRNA",
                    "snoRNA",
                    "snRNA",
                    "sRNA"
                ) ~ "small",
                .data[["biotype"]] %in% c(
                    "non_stop_decay",
                    "nonsense_mediated_decay"
                ) ~ "decaying",
                grepl(
                    pattern = "^ig_",
                    x = .data[["biotype"]],
                    ignore.case = TRUE
                ) ~ "ig",
                grepl(
                    pattern = "^tr_",
                    x = .data[["biotype"]],
                    ignore.case = TRUE
                ) ~ "tcr",
                TRUE ~ "other")
        ) %>%
        column_to_rownames()
}



#' @importFrom dplyr distinct everything group_by left_join mutate rename select
#'   summarize_all ungroup
#' @importFrom rlang !! sym
.genes.dataFrame <- function(x, uniqueSymbol = FALSE) {  # nolint
    assert_is_data.frame(x)
    assert_are_identical(colnames(x), ensembldbGeneCols)
    x %>%
        rename(
            ensgene = .data[["gene_id"]],
            biotype = .data[["gene_biotype"]],
            entrez = .data[["entrezid"]]
        ) %>%
        # Use `symbol` column instead of duplicate `gene_name` column
        mutate(gene_name = NULL) %>%
        camel(colnames = TRUE, rownames = FALSE) %>%
        .defineBroadClass() %>%
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



# Aliases ======================================================================
# Changed to an aliases in v0.3.2
#' @rdname genes
#' @export
annotable <- function(object, ...) {
    genes(x = object, ...)
}
