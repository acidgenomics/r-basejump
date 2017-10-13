#' Generate gene2symbol from GFF/GTF File
#'
#' @rdname gene2symbolFromGFF
#' @name gene2symbolFromGFF
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#'
#' @details The GFF (General Feature Format) format consists of one line per
#'   feature, each containing 9 columns of data, plus optional track definition
#'   lines. The GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @return [data.frame].
#'
#' @examples
#' # From URL (recommended)
#' url <- file.path(testDataURL, "mmusculus.gtf")
#' gene2symbolFromGFF(url) %>%
#'     str()
#'
#' # GFF data.frame
#' gff <- readGFF(url)
#' gene2symbolFromGFF(gff) %>%
#'     str()
NULL



# Constructors ====
.gene2symbolFromGFF <- function(object) {
    anno <- .gffKeyValuePairs(object)

    # Standard `gene_symbol` to `gene_name` (Ensembl format).
    # This fix is necessary for FlyBase GFF files.
    if (any(str_detect(anno, "gene_symbol"))) {
        message("Renaming 'gene_symbol' to 'gene_name'")
        anno <- str_replace_all(anno, "gene_symbol", "gene_name")
    }

    anno <- anno %>%
        .[str_detect(., "gene_id") & str_detect(., "gene_name")] %>%
        unique()

    ensgene <- str_match(anno, "gene_id ([^;]+);") %>%
        .[, 2L]
    symbol <- str_match(anno, "gene_name ([^;]+);") %>%
        .[, 2L]

    df <- cbind(ensgene, symbol) %>%
        as.data.frame() %>%
        distinct() %>%
        # Ensure unique symbols (not always the case -- e.g. human, mouse)
        mutate(symbol = make.unique(as.character(.data[["symbol"]]))) %>%
        arrange(!!sym("ensgene")) %>%
        set_rownames(.[["ensgene"]])

    message(paste("gene2symbol mappings:", nrow(df), "genes"))

    df
}



# Methods ====
#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("character"),
    function(object) {
        object %>%
            readGFF() %>%
            .gene2symbolFromGFF()
    })



#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("data.frame"),
    function(object) {
        .gene2symbolFromGFF(object)
    })
