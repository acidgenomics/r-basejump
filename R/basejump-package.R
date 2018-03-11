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
    "data.frame",
    "DataFrame"
)
geneAnnotationCols <- c(
    "geneID",
    "geneName",
    "description",
    "geneBiotype"
)
transcriptAnnotationCols <- c(
    "txID",
    geneAnnotationCols,
    "txBiotype"
)
