#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @importFrom BiocCheck BiocCheck
#' @importFrom BiocInstaller biocValid
#' @importFrom devtools build build_vignettes check document install load_all
#'   test
#' @importFrom dplyr arrange filter funs group_by left_join mutate mutate_all
#'   select summarise_all top_n
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_colnames set_names set_rownames
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom readr read_csv read_delim read_lines read_tsv write_csv
#' @importFrom readxl read_excel
#' @importFrom rlang !!! !! is_string sym syms
#' @importFrom rmarkdown render
#' @importFrom stats na.omit
#' @importFrom stringr str_replace_all str_subset
#' @importFrom tibble glimpse remove_rownames
#' @importFrom utils download.file globalVariables
"_PACKAGE"



globalVariables(c(".", "biocLite"))
