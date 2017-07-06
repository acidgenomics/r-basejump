#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' The package exports [camel()] and [snake()] variants of all functions,
#' for seamless integration into [Bioconductor](https://www.bioconductor.org)
#' and [tidyverse](http://tidyverse.org) workflows. Function parameters are
#' always passed in [camel()] case. We also provide British spelling variants
#' (e.g. colour) of the function names wherever applicable.
#'
#' @import AnnotationDbi Biobase BiocGenerics methods pbapply pbmcapply readr
#'   stringr SummarizedExperiment S4Vectors
#' @importFrom BiocInstaller biocValid
#' @importFrom biomaRt getBM listMarts useEnsembl useMart
#' @importFrom covr package_coverage
#' @importFrom cowplot plot_grid
#' @importFrom devtools build build_vignettes check document install load_all
#'   test update_packages use_data use_data_raw use_testthat
#' @importFrom dplyr arrange bind_cols bind_rows case_when desc distinct
#'   everything filter full_join funs group_by inner_join left_join mutate
#'   mutate_all n pull right_join summarise summarise_all summarize
#'   summarize_all top_n ungroup
#' @importFrom graphics hist
#' @importFrom httr content_type_json GET user_agent
#' @importFrom knitr asis_output kable knit opts_chunk opts_knit
#' @importFrom lintr lint_package
#' @importFrom magrittr %>% set_colnames set_rownames
#' @importFrom Matrix readMM writeMM
#' @importFrom parallel mclapply mcmapply
#' @importFrom R.utils gzip gunzip
#' @importFrom RCurl getURL
#' @importFrom readxl read_excel
#' @importFrom reshape2 melt
#' @importFrom rlang !!! !! .data eval_bare is_atomic is_bytes is_character
#'   is_double is_integer is_list is_logical is_null is_raw is_string is_vector
#'   quo quos set_names sym syms UQ
#' @importFrom rmarkdown render
#' @importFrom stats hclust
#' @importFrom tibble as_tibble column_to_rownames glimpse is_tibble
#'   remove_rownames rownames_to_column tibble
#' @importFrom tidyr expand_ nest nest_ separate separate_ unnest unnest_
#' @importFrom tools file_path_sans_ext
#' @importFrom utils download.file globalVariables object.size sessionInfo
#' @importFrom yaml yaml.load_file
"_PACKAGE"

globalVariables(c(".", "biocLite"))
