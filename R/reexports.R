# Bioconductor ====
#' @importFrom AnnotationDbi select
#' @usage NULL
#' @export
AnnotationDbi::select

#' @importFrom BiocGenerics colSums
#' @usage NULL
#' @usage NULL
#' @export
BiocGenerics::colSums

#' @importFrom BiocInstaller biocValid
#' @usage NULL
#' @export
BiocInstaller::biocValid



# biomaRt ====
#' @importFrom biomaRt getBM listMarts useEnsembl

#' @usage NULL
#' @export
biomaRt::getBM

#' @usage NULL
#' @export
biomaRt::listMarts

#' @usage NULL
#' @export
biomaRt::useEnsembl



# devtools ====
#' @importFrom devtools build build_vignettes check document install load_all
#'   test

#' @usage NULL
#' @export
devtools::build

#' @usage NULL
#' @export
devtools::build_vignettes

#' @usage NULL
#' @export
devtools::check

#' @usage NULL
#' @export
devtools::document

#' @usage NULL
#' @export
devtools::install

#' @usage NULL
#' @export
devtools::load_all

#' @usage NULL
#' @export
devtools::test



# dplyr ====
#' @importFrom dplyr funs group_by left_join mutate_all summarise_all top_n

#' @rdname tidyverse
#' @description Arrange rows by variables.
#' @export
dplyr::arrange -> tidy_arrange

#' @rdname tidyverse
#' @description Return rows with matching conditions.
#' @export
dplyr::filter -> tidy_filter

#' @usage NULL
#' @export
dplyr::funs

#' @usage NULL
#' @export
dplyr::group_by

#' @usage NULL
#' @export
dplyr::left_join

#' @rdname tidyverse
#' @description Add new variables.
#' @export
dplyr::mutate -> tidy_mutate

#' @usage NULL
#' @export
dplyr::mutate_all

#' @rdname tidyverse
#' @description Select/rename variables by name.
#' @export
dplyr::select -> tidy_select

#' @usage NULL
#' @export
dplyr::summarise_all

#' @usage NULL
#' @export
dplyr::top_n



# graphics ====
#' @importFrom graphics hist
#' @usage NULL
#' @export
graphics::hist



# knitr ====
#' @importFrom knitr asis_output kable opts_knit



# magrittr ====
#' @importFrom magrittr %>% set_colnames set_names set_rownames

#' @usage NULL
#' @export
magrittr::`%>%`

#' @usage NULL
#' @export
magrittr::set_colnames

#' @usage NULL
#' @export
magrittr::set_names

#' @usage NULL
#' @export
magrittr::set_rownames



# methods ====
#' @importFrom methods as show

#' @usage NULL
#' @export
methods::as

#' @usage NULL
#' @export
methods::show



# R.utils ====
#' @importFrom R.utils gzip gunzip

#' @usage NULL
#' @export
R.utils::gunzip

#' @usage NULL
#' @export
R.utils::gzip



# RCurl ====
#' @importFrom RCurl getURL

#' @usage NULL
#' @export
RCurl::getURL



# readr ====
#' @importFrom readr read_csv read_delim read_lines read_tsv write_csv

#' @usage NULL
#' @export
readr::read_csv

#' @usage NULL
#' @export
readr::read_delim

#' @usage NULL
#' @export
readr::read_lines

#' @usage NULL
#' @export
readr::read_tsv

#' @usage NULL
#' @export
readr::write_csv



# readxl ====
#' @importFrom readxl read_excel

#' @usage NULL
#' @export
readxl::read_excel



# rlang ====
#' @importFrom rlang !!! !! is_string sym syms

#' @usage NULL
#' @export
rlang::`!!!`

#' @usage NULL
#' @export
rlang::`!!`

#' @usage NULL
#' @export
rlang::is_string

#' @usage NULL
#' @export
rlang::sym

#' @usage NULL
#' @export
rlang::syms



# rmarkdown ====
#' @importFrom rmarkdown render

#' @usage NULL
#' @export
rmarkdown::render



# stats ====
#' @importFrom stats aggregate median na.omit setNames

#' @usage NULL
#' @export
stats::aggregate

#' @usage NULL
#' @export
stats::median

#' @usage NULL
#' @export
stats::na.omit

#' @usage NULL
#' @export
stats::setNames



# stringr ====
#' @import stringr

#' @usage NULL
#' @export
stringr::str_detect

#' @usage NULL
#' @export
stringr::str_extract

#' @usage NULL
#' @export
stringr::str_extract_all

#' @usage NULL
#' @export
stringr::str_match

#' @usage NULL
#' @export
stringr::str_match_all

#' @usage NULL
#' @export
stringr::str_replace

#' @usage NULL
#' @export
stringr::str_replace_all

#' @usage NULL
#' @export
stringr::str_subset



# tibble ====
#' @importFrom tibble as_tibble glimpse remove_rownames tibble

#' @usage NULL
#' @export
tibble::as_tibble

#' @usage NULL
#' @export
tibble::glimpse

#' @usage NULL
#' @export
tibble::remove_rownames

#' @usage NULL
#' @export
tibble::tibble



# tidyr ====
#' @importFrom tidyr expand_ nest separate_ unnest

#' @usage NULL
#' @export
tidyr::expand_

#' @usage NULL
#' @export
tidyr::nest

#' @usage NULL
#' @export
tidyr::separate_

#' @usage NULL
#' @export
tidyr::unnest



# utils ====
#' @importFrom utils download.file globalVariables

#' @usage NULL
#' @export
utils::download.file

#' @usage NULL
#' @export
utils::globalVariables
