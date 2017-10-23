#' Generate gene2symbol from GFF/GTF File
#'
#' @rdname gene2symbolFromGFF
#' @name gene2symbolFromGFF
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
#'
#' @details The GFF (General Feature Format) format consists of one line per
#'   feature, each containing 9 columns of data, plus optional track definition
#'   lines. The GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @return [data.frame].
#'
#' @examples
#' # From URL (recommended)
#' url <- "http://basejump.seq.cloud/mmusculus.gtf"
#' gene2symbolFromGFF(url) %>% str()
#'
#' # GFF data.frame
#' gff <- readGFF(url)
#' gene2symbolFromGFF(gff) %>% str()
NULL



# Constructors ====
#' @importFrom dplyr arrange mutate
#' @importFrom magrittr set_rownames
#' @importFrom rlang .data sym !!
#' @importFrom stringr str_match
.gene2symbolFromGFF <- function(
    object,
    quiet = FALSE) {
    anno <- .gffKeyValuePairs(object)

    # Standard `gene_symbol` to `gene_name` (Ensembl format).
    # This fix is necessary for FlyBase GFF files.
    if (any(grepl(x = anno, pattern = "gene_symbol"))) {
        anno <- gsub(
            x = anno,
            pattern = "gene_symbol",
            replacement = "gene_name")
    }

    anno <- anno %>%
        .[grepl(x = ., pattern = "gene_id") &
              grepl(x = ., pattern = "gene_name")] %>%
        unique()

    ensgene <- str_match(
        anno,
        pattern = "gene_id ([^;]+);") %>%
        .[, 2L]
    symbol <- str_match(
        anno,
        pattern = "gene_name ([^;]+);") %>%
        .[, 2L]

    df <- cbind(ensgene, symbol) %>%
        as.data.frame() %>%
        distinct() %>%
        # Ensure unique symbols (not always the case -- e.g. human, mouse)
        mutate(symbol = make.unique(as.character(.data[["symbol"]]))) %>%
        arrange(!!sym("ensgene")) %>%
        set_rownames(.[["ensgene"]])

    if (!isTRUE(quiet)) {
        message(paste("gene2symbol mappings:", nrow(df), "genes"))
    }

    df
}



# Methods ====
#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        object %>%
            readGFF(quiet = quiet) %>%
            .gene2symbolFromGFF(quiet = quiet)
    })



#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("data.frame"),
    function(
        object,
        quiet = FALSE) {
        .gene2symbolFromGFF(object, quiet = quiet)
    })
