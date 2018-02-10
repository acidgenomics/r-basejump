#' Define Gene to Symbol Mappings from GFF/GTF File
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
#' gene2symbolFromGFF(url) %>% glimpse()
#'
#' # GFF data.frame
#' gff <- readGFF(url)
#' gene2symbolFromGFF(gff) %>% glimpse()
NULL



# Constructors =================================================================
#' @importFrom dplyr arrange mutate
#' @importFrom magrittr set_rownames
#' @importFrom stringr str_match
.gene2symbolFromGFF <- function(
    object,
    quiet = FALSE) {
    .checkQuiet(quiet)

    anno <- .gffKeyValuePairs(object)

    # Standard `gene_symbol` to `gene_name` (Ensembl format).
    # This fix is necessary for FlyBase GFF files.
    if (any(grepl("gene_symbol", anno))) {
        anno <- gsub("gene_symbol", "gene_name", anno)
    }

    anno <- anno %>%
        .[grepl("gene_id", .) & grepl("gene_name", .)] %>%
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
        as.data.frame(stringsAsFactors = FALSE) %>%
        distinct() %>%
        # Ensure unique symbols (not always the case -- e.g. human, mouse)
        mutate(symbol = make.unique(as.character(.data[["symbol"]]))) %>%
        arrange(!!sym("ensgene")) %>%
        set_rownames(.[["ensgene"]])

    if (!isTRUE(quiet)) {
        inform(paste("gene2symbol mappings:", nrow(df), "genes"))
    }

    df
}



# Methods ======================================================================
#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("character"),
    function(
        object,
        quiet = FALSE) {
        # Passthrough: quiet
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
        # Passthrough: quiet
        .gene2symbolFromGFF(object, quiet = quiet)
    })
