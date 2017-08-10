#' Generate gene2symbol from GTF File
#'
#' @rdname gene2symbolFromGTF
#' @name gene2symbolFromGTF
#'
#' @return [data.frame].
#'
#' @examples
#' # Mouse (Ensembl)
#' gene2symbolFromGTF("http://steinbaugh.com/basejump/tests/mmusculus.gtf")
#'
#' # Fruitfly (FlyBase)
#' gene2symbolFromGTF("http://steinbaugh.com/basejump/tests/dmelanogaster.gtf")
NULL



# Constructors ====
.gene2symbolFromGTF <- function(object) {
    anno <- .annotationsFromGTF(object)

    # Standard `gene_symbol` to `gene_name` (Ensembl format).
    # This fix is necessary for FlyBase GTF files.
    if (any(str_detect(anno, "gene_symbol"))) {
        message("Renaming 'gene_symbol' to 'gene_name'")
        anno <- str_replace_all(anno, "gene_symbol", "gene_name")
    }

    anno <- anno %>%
        .[str_detect(., "gene_id") & str_detect(., "gene_name")] %>%
        unique

    ensgene <- str_match(anno, "gene_id \"([^\"]+)\"") %>%
        .[, 2L]
    symbol <- str_match(anno, "gene_name \"([^\"]+)\"") %>%
        .[, 2L]

    # Check identifier integrity
    if (!identical(length(ensgene), length(symbol))) {
        stop("Gene/symbol mismatch")
    }

    df <- cbind(ensgene, symbol) %>%
        as.data.frame %>%
        distinct %>%
        # Ensure unique symbols (not always the case -- e.g. human, mouse)
        mutate(symbol = make.unique(as.character(.data[["symbol"]]))) %>%
        arrange(!!sym("ensgene")) %>%
        set_rownames(.[["ensgene"]])

    # Ensure that symbols are unique
    if (any(duplicated(df[["symbol"]]))) {
        stop("Persistent duplicate symbols")
    }

    message(paste("gene2symbol mappings:", nrow(df), "genes"))

    df
}



# Methods ====
#' @rdname gene2symbolFromGTF
#' @export
setMethod("gene2symbolFromGTF", "character", function(object) {
    # Check for remote file
    if (str_detect(object, "://")) {
        # Save as temp file
        file <- tempfile()
        download.file(object, file)
    } else {
        file <- object
    }
    .gene2symbolFromGTF(file)
})
