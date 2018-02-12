#' Define Gene to Symbol Mappings from GFF/GTF File
#'
#' @rdname gene2symbolFromGFF
#' @name gene2symbolFromGFF
#' @family Gene Annotation Utilities
#'
#' @inheritParams general
#' @inheritParams gene2symbol
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
#' @importFrom rlang !! .data sym
#' @importFrom stringr str_match
.gene2symbolFromGFF <- function(
    object,
    uniqueSymbol = FALSE,
    quiet = FALSE) {
    assert_is_data.frame(object)
    assert_is_a_bool(uniqueSymbol)
    assert_is_a_bool(quiet)

    anno <- .gffKeyValuePairs(object)
    assert_is_character(anno)

    # Standard `gene_symbol` to `gene_name` (Ensembl format).
    # This fix is necessary for FlyBase GFF files.
    if (any(grepl("gene_symbol", anno))) {
        anno <- gsub("gene_symbol", "gene_name", anno)
    }

    anno <- anno %>%
        .[grepl("gene_id", .)] %>%
        .[grepl("gene_name", .)] %>%
        unique()

    ensgene <- str_match(anno, "gene_id ([^;]+);") %>%
        .[, 2L]
    symbol <- str_match(anno, "gene_name ([^;]+);") %>%
        .[, 2L]

    assert_all_are_non_missing_nor_empty_character(ensgene)
    assert_all_are_non_missing_nor_empty_character(symbol)
    assert_are_same_length(ensgene, symbol)

    if (!isTRUE(quiet)) {
        inform(paste("gene2symbol mappings:", length(ensgene), "genes"))
    }

    data <- cbind(ensgene, symbol) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        distinct() %>%
        arrange(!!sym("ensgene"))

    if (isTRUE(uniqueSymbol)) {
        data <- mutate(data, symbol = make.unique(.data[["symbol"]]))
    }

    set_rownames(data, data[["ensgene"]])
}



# Methods ======================================================================
#' @rdname gene2symbolFromGFF
#' @export
setMethod(
    "gene2symbolFromGFF",
    signature("character"),
    function(object, quiet = FALSE) {
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
    function(object, quiet = FALSE) {
        # Passthrough: quiet
        .gene2symbolFromGFF(object, quiet = quiet)
    })
