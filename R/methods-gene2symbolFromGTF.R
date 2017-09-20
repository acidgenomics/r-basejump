#' Generate gene2symbol from GTF File
#'
#' @rdname gene2symbolFromGTF
#' @name gene2symbolFromGTF
#'
#' @return [data.frame].
#'
#' @examples
#' # GTF URL
#' url <- file.path(testDataURL, "mmusculus.gtf")
#' gene2symbolFromGTF(url) %>%
#'     str
#'
#' # GTF data.frame
#' gtf <- readGTF(url)
#' gene2symbolFromGTF(gtf) %>%
#'     str
NULL



# Constructors ====
.gene2symbolFromGTF <- function(object) {
    anno <- .gtfKeyValuePairs(object)

    # Standard `gene_symbol` to `gene_name` (Ensembl format).
    # This fix is necessary for FlyBase GTF files.
    if (any(str_detect(anno, "gene_symbol"))) {
        message("Renaming 'gene_symbol' to 'gene_name'")
        anno <- str_replace_all(anno, "gene_symbol", "gene_name")
    }

    anno <- anno %>%
        .[str_detect(., "gene_id") & str_detect(., "gene_name")] %>%
        unique

    ensgene <- str_match(anno, "gene_id ([^;]+);") %>%
        .[, 2L]
    symbol <- str_match(anno, "gene_name ([^;]+);") %>%
        .[, 2L]

    df <- cbind(ensgene, symbol) %>%
        as.data.frame %>%
        distinct %>%
        # Ensure unique symbols (not always the case -- e.g. human, mouse)
        mutate(symbol = make.unique(as.character(.data[["symbol"]]))) %>%
        arrange(!!sym("ensgene")) %>%
        set_rownames(.[["ensgene"]])

    message(paste("gene2symbol mappings:", nrow(df), "genes"))

    df
}



# Methods ====
#' @rdname gene2symbolFromGTF
#' @export
setMethod("gene2symbolFromGTF", "character", function(object) {
    object %>%
        readGTF %>%
        .gene2symbolFromGTF
})



#' @rdname gene2symbolFromGTF
#' @export
setMethod("gene2symbolFromGTF", "data.frame", function(object) {
    .gene2symbolFromGTF(object)
})
