#' [Ensembl](http://www.ensembl.org/) annotations
#'
#' Quickly access gene annotations and transcript-to-gene (tx2gene) mappings
#' pre-compiled from [Ensembl](http://www.ensembl.org/) with the
#' [biomaRt](http://bioconductor.org/packages/release/bioc/html/biomaRt.html)
#' package. For gene annotables, the Entrez identifier is removed, to allow
#' for unique Ensembl gene identifiers.
#'
#' @rdname annotables
#' @docType methods
#' @author Michael Steinbaugh, Rory Kirchner
#'
#' @param object Object. Default usage is to provide Ensembl genome build as a
#'   string.
#'
#' @note If the `format` argument is set to `gene2entrez`, [annotable()] returns
#'   a [tibble] with non-unique rows grouped by `ensgene`, instead of a
#'   [data.frame].
#' @seealso Consult the annotables package documentation (`help("annotables")`)
#'   for a list of currently supported genomes.
#'
#' @return [data.frame] with unique rows per gene or transcript.



#' @rdname annotables
#' @usage NULL
## @param genomeBuild Genome build.
## @param format Desired table format, either `gene`, `tx2gene`, `gene2symbol`,
##   or `gene2entrez`.
.annotable <- function(genomeBuild, format) {
    if (!is.character(genomeBuild)) {
        stop("Genome build must be a character vector")
    }
    if (!format %in% c("gene", "tx2gene", "gene2symbol", "gene2entrez")) {
        stop("Unsupported format")
    }
    envir <- as.environment("package:annotables")

    # Remap genome build aliases
    if (genomeBuild == "hg19") {
        genomeBuild <- "grch37"
    } else if (genomeBuild == "hg38") {
        genomeBuild <- "grch38"
    } else if (genomeBuild == "mm10") {
        genomeBuild <- "grcm38"
    }

    message(paste("Using", genomeBuild, format, "annotable"))

    if (format == "gene") {
        get(genomeBuild, envir = envir) %>%
            mutate(entrez = NULL) %>%
            distinct %>%
            arrange(.data[["ensgene"]]) %>%
            mutate(symbol = make.unique(.data[["symbol"]]),
                   broad_class = case_when(
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
                       str_detect(.data[["biotype"]], "IG_") ~ "ig",
                       str_detect(.data[["biotype"]], "TR_") ~ "tcr",
                       TRUE ~ "other")) %>%
            as.data.frame %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "tx2gene") {
        str_c(genomeBuild, "tx2gene", sep = "_") %>%
            get(envir = envir) %>%
            arrange(.data[["enstxp"]]) %>%
            as.data.frame %>%
            set_rownames(.[["enstxp"]])
    } else if (format == "gene2symbol") {
        get(genomeBuild, envir = envir) %>%
            tidy_select(c("ensgene", "symbol")) %>%
            distinct %>%
            arrange(.data[["ensgene"]]) %>%
            mutate(symbol = make.unique(.data[["symbol"]])) %>%
            as.data.frame %>%
            set_rownames(.[["ensgene"]])
    } else if (format == "gene2entrez") {
        get(genomeBuild, envir = envir) %>%
            tidy_select(c("ensgene", "entrez")) %>%
            filter(!is.na(.data[["entrez"]])) %>%
            group_by(.data[["ensgene"]]) %>%
            arrange(.data[["entrez"]], .by_group = TRUE)
    }
}



#' @rdname annotables
#' @export
setMethod("annotable", "character", function(object) {
    if (!is_string(object)) {
        stop("Genome build must be specified as string")
    }
    .annotable(genomeBuild = object, format = "gene")
})



#' @rdname annotables
#' @export
setMethod("gene2entrez", "character", function(object) {
    if (!is_string(object)) {
        stop("Genome build must be specified as string")
    }
    .annotable(genomeBuild = object, format = "gene2entrez")
})



#' @rdname annotables
#' @export
setMethod("gene2symbol", "character", function(object) {
    if (!is_string(object)) {
        stop("Genome build must be specified as string")
    }
    .annotable(genomeBuild = object, format = "gene2symbol")
})



#' @rdname annotables
#' @export
setMethod("tx2gene", "character", function(object) {
    if (!is_string(object)) {
        stop("Genome build must be specified as string")
    }
    .annotable(genomeBuild = object, format = "tx2gene")
})
