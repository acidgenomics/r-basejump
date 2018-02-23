#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import methods
#' @importFrom rlang abort inform warn
#' @importFrom S4Vectors mcols tail
#' @importFrom utils capture.output globalVariables
"_PACKAGE"



globalVariables(".")
annotableCols <- c(
    "ensgene",
    "symbol",
    "description",
    "biotype")
ensembldbGeneCols <- c(
    "gene_id",
    "gene_name",
    "gene_biotype",
    "gene_seq_start",
    "gene_seq_end",
    "seq_name",
    "seq_strand",
    "seq_coord_system",
    "description",
    "symbol",
    "entrezid")
