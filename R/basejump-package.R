#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import methods
#' @importFrom rlang abort inform warn
#' @importFrom S4Vectors mcols mcols<- tail
#' @importFrom utils capture.output globalVariables
"_PACKAGE"



globalVariables(".")
ensemblReturn <- c(
    "GRanges",
    "DataFrame",
    "data.frame"
)
annotationCols <- c(
    "txID",
    "txName",
    "txBiotype",
    "geneID",
    "geneName",
    "geneBiotype",
    "broadClass",
    "description"
)
