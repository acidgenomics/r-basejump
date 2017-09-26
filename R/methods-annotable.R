#' [Ensembl](http://www.ensembl.org/) Annotations
#'
#' Quickly access gene annotations and transcript-to-gene (tx2gene) mappings
#' pre-compiled from [Ensembl](http://www.ensembl.org/) with the
#' [biomaRt](http://bioconductor.org/packages/release/bioc/html/biomaRt.html)
#' package. For gene annotables, the Entrez identifier is removed, to allow
#' for unique Ensembl gene identifiers.
#'
#' @rdname annotable
#' @name annotable
#' @author Broad class definitions by Rory Kirchner
#'
#' @param object Object. Default usage is to provide Ensembl genome build as a
#'   string.
#' @param format Desired table format, either `gene`, `tx2gene`, or
#'   `gene2symbol`.
#' @param release Ensembl release version. This function defaults to using the
#'   most current release available on AnnotationHub (`current`).
#'
#' @return [data.frame] with unique rows per gene or transcript.
#'
#' @examples
#' annotable("Mus musculus") %>% str
NULL



# Constructors ====
.annotable <- function(object, format = "gene", release = "current") {
    if (!is_string(object)) {
        stop("Object must be a string")
    }
    if (!format %in% c("gene", "gene2symbol", "tx2gene")) {
        stop("Unsupported format")
    }

    organism <- detectOrganism(object)

    # Download organism EnsDb package from AnnotationHub
    message("Obtaining Ensembl annotations with AnnotationHub and ensembldb")
    getAnnotationHubOption("CACHE") %>%
        normalizePath() %>%
        message()
    ah <- AnnotationHub()
    if (release == "current") {
        ahDb <- query(
            ah,
            pattern = c(organism, "EnsDb"),
            ignore.case = TRUE)
        # Get the latest AnnotationHub dataset by identifier number
        id <- ahDb %>%
            mcols %>%
            rownames %>%
            tail(n = 1L)
        edb <- suppressMessages(ah[[id]])
    } else {
        ahDb <- query(
            ah,
            pattern = c(organism,
                        "EnsDb",
                        # Match against the version more specifically
                        # (e.g. "v90")
                        paste0("v", release)),
            ignore.case = TRUE)
        edb <- suppressMessages(ahDb[[1L]])
    }

    message(paste("EnsDB:",
                  organism(edb),
                  "Ensembl",
                  ensemblVersion(edb)))

    if (format == "gene") {
        genes(edb,
              columns = c("gene_id",
                          "symbol",  # `gene_name` also works
                          "description",
                          "gene_biotype"),
              return.type = "data.frame") %>%
            dplyr::rename(ensgene = .data[["gene_id"]],
                          biotype = .data[["gene_biotype"]]) %>%
            dplyr::mutate(
                # Ensure unique symbols (e.g. human, mouse)
                symbol = make.unique(.data[["symbol"]]),
                # Define the broad class
                broadClass = case_when(
                    str_detect(
                        .data[["symbol"]],
                        # Hsapiens: `MT-`,
                        # Mmusculus: `mt-`
                        # Dmelanogaster: `mt:`
                        regex("^MT[\\:\\-]",
                              ignore_case = TRUE)) ~ "mito",
                    .data[["biotype"]] == "protein_coding" ~ "coding",
                    .data[["biotype"]] %in%
                        c("known_ncrna",
                          "lincRNA",
                          "non_coding") ~ "noncoding",
                    str_detect(.data[["biotype"]], "pseudo") ~ "pseudo",
                    .data[["biotype"]] %in%
                        c("miRNA",
                          "misc_RNA",
                          "ribozyme",
                          "rRNA",
                          "scaRNA",
                          "scRNA",
                          "snoRNA",
                          "snRNA",
                          "sRNA") ~ "small",
                    .data[["biotype"]] %in%
                        c("non_stop_decay",
                          "nonsense_mediated_decay") ~ "decaying",
                    str_detect(.data[["biotype"]],
                               regex("^ig_", ignore_case = TRUE)) ~ "ig",
                    str_detect(.data[["biotype"]],
                               regex("^tr_", ignore_case = TRUE)) ~ "tcr",
                    TRUE ~ "other")) %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "gene2symbol") {
        genes(edb,
              columns = c("gene_id", "symbol"),
              return.type = "data.frame") %>%
            dplyr::rename(ensgene = .data[["gene_id"]]) %>%
            # Ensure unique symbols (e.g. human, mouse)
            dplyr::mutate(symbol = make.unique(.data[["symbol"]])) %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "tx2gene") {
        transcripts(edb,
                    columns = c("tx_id", "gene_id"),
                    return.type = "data.frame") %>%
            dplyr::rename(enstxp = .data[["tx_id"]],
                          ensgene = .data[["gene_id"]]) %>%
            set_rownames(.[["enstxp"]])
    }
}



# Methods ====
#' @rdname annotable
#' @export
setMethod("annotable", "character", .annotable)
