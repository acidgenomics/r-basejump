#' basejump
#'
#' Base functions for bioinformatics and R package development.
#'
#' @import Biobase BiocGenerics datasets methods S4Vectors
#' @importFrom AnnotationHub AnnotationHub getAnnotationHubOption query
#' @importFrom data.table rbindlist
#' @importFrom devtools session_info
#' @importFrom dplyr arrange bind_cols case_when distinct everything filter funs
#'   group_by left_join mutate mutate_all mutate_if pull summarize_all ungroup
#' @importFrom ensembldb ensemblVersion genes transcripts
#' @importFrom glue collapse
#' @importFrom knitr asis_output kable opts_knit
#' @importFrom magrittr %>% set_colnames set_rownames
#' @importFrom Matrix Matrix readMM writeMM
#' @importFrom R.utils gzip
#' @importFrom RCurl getURL
#' @importFrom readr read_csv read_delim read_lines read_tsv write_csv
#'   write_lines write_tsv
#' @importFrom readxl read_excel
#' @importFrom rlang .data dots_list eval_bare is_string sym syms
#' @importFrom stats setNames
#' @importFrom stringr regex str_detect str_dup str_extract str_match str_pad
#'   str_replace str_replace_all str_replace_na str_split str_subset
#' @importFrom SummarizedExperiment colData SummarizedExperiment
#' @importFrom tibble column_to_rownames has_rownames is_tibble
#'   rownames_to_column tibble
#' @importFrom utils download.file globalVariables read.delim read.table
#'   sessionInfo
#' @importFrom yaml yaml.load_file
"_PACKAGE"

globalVariables(".")

metaPriorityCols <- c("sampleID", "sampleName")

# Use this for `data-raw/` scripts to define output path
testDataDir <- file.path("docs", "tests")

#' Test Data URL
#' @keywords internal
#' @export
testDataURL <- file.path(
    "https://raw.githubusercontent.com",
    "steinbaugh",
    "basejump",
    "develop",
    "docs",
    "tests")

#' Package Website URL
#' @keywords internal
#' @export
url <- "http://steinbaugh.com/basejump"
