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
#' @param format Desired table format, either **`gene`**, `tx2gene`,
#'   `gene2symbol`, or `gene2entrez`.
#'
#' @note If the `format` argument is set to `gene2entrez`, [annotable()] returns
#'   a [tibble] with non-unique rows grouped by `ensgene`, instead of a
#'   [data.frame].
#' @seealso Consult the annotables package documentation (`help("annotables")`)
#'   for a list of currently supported genomes.
#'
#' @return [data.frame] with unique rows per gene or transcript.
#'
#' @examples
#' annotable("hg38") %>% glimpse
NULL



# Constructors ====
.annotable <- function(object, format = "gene") {
    string <- object[1L]  # nolint

    if (!format %in% c("gene", "tx2gene", "gene2symbol", "gene2entrez")) {
        stop("Unsupported format")
    }
    envir <- as.environment("package:annotables")

    supportedGenomes <-
        c(chicken = "galgal5",
          fruitfly = "bdgp6",
          human = "grch38",
          human37 = "grch37",
          mouse = "grcm38",
          rat = "rnor6",
          roundworm = "wbcel235")

    if (!is.null(names(string))) {
        # `detectOrganism()` support
        # Get genome build by organism name
        string <- names(string)
        if (string %in% names(supportedGenomes)) {
            genome <- supportedGenomes[[string]]
        } else {
            stop("Unsupported organism name")
        }
    } else {
        string <- tolower(string)
        if (string == "hg19") {
            genome <- "grch37"
        } else if (string == "hg38") {
            genome <- "grch38"
        } else if (string == "mm10") {
            genome <- "grcm38"
        } else {
            genome <- string
        }
        if (!genome %in% supportedGenomes) {
            stop("String failed to match a supported genome")
        }
    }

    if (format == "gene") {
        get(genome, envir = envir) %>%
            mutate(entrez = NULL) %>%
            distinct %>%
            arrange(.data[["ensgene"]]) %>%
            mutate(
                # Ensure unique symbols
                symbol = make.unique(.data[["symbol"]]),
                broadClass = case_when(
                    # Chromosome
                    str_detect(.data[["chr"]],
                               regex("mito|mt",
                                     ignore_case = TRUE)) ~ "mito",
                    # Biotype
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
            as.data.frame %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "tx2gene") {
        paste(genome, "tx2gene", sep = "_") %>%
            get(envir = envir) %>%
            arrange(.data[["enstxp"]]) %>%
            as.data.frame %>%
            set_rownames(.[["enstxp"]])
    } else if (format == "gene2symbol") {
        get(genome, envir = envir) %>%
            tidy_select(c("ensgene", "symbol")) %>%
            distinct %>%
            arrange(.data[["ensgene"]]) %>%
            mutate(symbol = make.unique(.data[["symbol"]])) %>%
            as.data.frame %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "gene2entrez") {
        get(genome, envir = envir) %>%
            tidy_select(c("ensgene", "entrez")) %>%
            filter(!is.na(.data[["entrez"]])) %>%
            group_by(.data[["ensgene"]]) %>%
            arrange(.data[["entrez"]], .by_group = TRUE)
    }
}



# Methods ====
#' @rdname annotable
#' @export
setMethod("annotable", "character", .annotable)
