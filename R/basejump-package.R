#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import BiocGenerics S4Vectors datasets methods
#'
#' @importFrom AnnotationHub AnnotationHub query snapshotDate
#' @importFrom Matrix Matrix readMM writeMM
#' @importFrom Matrix.utils aggregate.Matrix
#' @importFrom R.utils gzip
#' @importFrom RColorBrewer brewer.pal
#' @importFrom RCurl getURL
#' @importFrom cowplot plot_grid
#' @importFrom dendsort dendsort
#' @importFrom dplyr case_when funs mutate_all mutate_if summarize_all
#' @importFrom ensembldb ensemblVersion
#' @importFrom ggplot2 aes element_blank element_line element_rect element_text
#'   ggplot theme theme_minimal
#' @importFrom grDevices colorRampPalette
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_names set_rownames
#' @importFrom pheatmap pheatmap
#' @importFrom readr read_csv read_lines read_tsv write_csv write_lines
#' @importFrom readxl read_excel
#' @importFrom rlang abort dots_list eval_bare inform warn
#' @importFrom stats dist hclust quantile
#' @importFrom stringr str_dup str_extract str_length str_match str_pad
#'   str_replace_na str_subset
#' @importFrom tibble as_tibble column_to_rownames glimpse is_tibble
#'   rownames_to_column tibble
#' @importFrom viridis scale_color_viridis scale_fill_viridis viridis
#' @importFrom utils capture.output download.file globalVariables
#'   installed.packages packageVersion read.delim read.table
#' @importFrom yaml yaml.load_file
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
    "description"
)
