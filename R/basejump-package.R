#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import annotables AnnotationDbi Biobase BiocGenerics datasets methods
#'   MultiAssayExperiment pbapply pbmcapply readr stringr SummarizedExperiment
#'   S4Vectors
#' @importFrom cowplot draw_plot ggdraw plot_grid
#' @importFrom dplyr arrange bind_cols bind_rows case_when desc distinct
#'   everything filter full_join funs group_by inner_join left_join mutate
#'   mutate_all mutate_if n pull right_join summarise summarise_all summarise_if
#'   summarize summarize_all summarize_if top_n ungroup
#' @importFrom ggrepel geom_label_repel geom_text_repel
#' @importFrom glue collapse
#' @importFrom graphics hist
#' @importFrom httr content_type_json GET user_agent
#' @importFrom knitr asis_output kable knit opts_chunk opts_knit
#' @importFrom magrittr %$% %>% set_colnames set_rownames
#' @importFrom Matrix readMM writeMM
#' @importFrom parallel mclapply mcmapply
#' @importFrom R.utils gzip gunzip
#' @importFrom RCurl getURL
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom rlang !!! !! .data dots_list dots_values eval_bare eval_tidy
#'   is_atomic is_bytes is_character is_double is_integer is_list is_logical
#'   is_null is_raw is_string is_vector quo quos set_names sym syms UQ
#' @importFrom rmarkdown render
#' @importFrom stats formula hclust setNames
#' @importFrom tibble as_tibble column_to_rownames glimpse has_rownames
#'   is_tibble remove_rownames rownames_to_column tibble
#' @importFrom tidyr expand_ nest nest_ separate separate_ unnest unnest_
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file globalVariables object.size packageVersion
#'   sessionInfo
#' @importFrom yaml yaml.load_file
#' @importClassesFrom Matrix dgCMatrix dgTMatrix
"_PACKAGE"

globalVariables(".")
