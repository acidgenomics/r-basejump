# Bioconductor ====
#' @importFrom BiocInstaller biocValid
#' @export
BiocInstaller::biocValid



# biomaRt ====
#' @importFrom biomaRt getBM listMarts useEnsembl
#' @export
biomaRt::getBM

#' @export
biomaRt::listMarts

#' @export
biomaRt::useEnsembl



# devtools ====
#' @import devtools
#' @export
devtools::build

#' @export
devtools::build_vignettes

#' @export
devtools::check

#' @export
devtools::document

#' @export
devtools::install

#' @export
devtools::load_all

#' @export
devtools::test



## dplyr ====
#' @importFrom dplyr bind_cols bind_rows case_when combine desc distinct
#'   everything funs group_by left_join mutate_all summarise summarise_all
#'   summarize summarize_all top_n ungroup
#' @export
dplyr::bind_cols

#' @export
dplyr::bind_rows

#' @export
dplyr::case_when

#' @export
dplyr::combine

#' @export
dplyr::desc

#' @export
dplyr::distinct

#' @export
dplyr::everything

#' @export
dplyr::funs

#' @export
dplyr::group_by

#' @export
dplyr::left_join

#' @export
dplyr::mutate_all

#' Star Wars dataset.
#' @keywords internal
#' @export
dplyr::"starwars" -> "starwars"

#' @export
dplyr::summarise

#' @export
dplyr::summarise_all

#' @export
dplyr::summarize

#' @export
dplyr::summarize_all

#' @export
dplyr::top_n

#' @export
dplyr::ungroup



# graphics ====
#' @importFrom graphics hist
#' @export
graphics::hist



# knitr ====
#' @importFrom knitr asis_output kable opts_chunk opts_knit
#' @export
knitr::asis_output

#' @export
knitr::kable

#' @export
knitr::opts_chunk

#' @export
knitr::opts_knit



# magrittr ====
#' @importFrom magrittr %>% set_colnames set_rownames
#' @export
magrittr::`%>%`

#' @export
magrittr::set_colnames

#' @rdname aliases
#' @usage NULL
#' @export
magrittr::set_colnames -> setColnames

#' @export
magrittr::set_rownames

#' @rdname aliases
#' @usage NULL
#' @export
magrittr::set_rownames -> setRownames



# methods ====
#' @importFrom methods as show
#' @export
methods::as

#' @export
methods::show



# purrr ====
#' @importFrom purrr set_names
#' @export
purrr::set_names

# Don't export camel variant over stats!
purrr::set_names -> setNames



# R.utils ====
#' @importFrom R.utils gzip gunzip
#' @export
R.utils::gunzip

#' @export
R.utils::gzip



# RCurl ====
#' @importFrom RCurl getURL
#' @export
RCurl::getURL



# readr ====
#' @import readr
#' @export
readr::read_csv

#' @export
readr::read_delim

#' @export
readr::read_lines

#' @export
readr::read_table

#' @export
readr::read_tsv

#' @export
readr::write_csv

#' @export
readr::write_lines

#' @export
readr::write_tsv



# readxl ====
#' @importFrom readxl read_excel
#' @export
readxl::read_excel



# reshape ====
#' @importFrom reshape2 melt
#' @export
reshape2::melt



# rlang ====
#' @importFrom rlang !!! !! .data is_string quo quos sym syms UQ
#' @export
rlang::`!!!`

#' @export
rlang::`!!`

#' @export
rlang::.data

#' @export
rlang::is_string

#' @export
rlang::quo

#' @export
rlang::quos

#' @export
rlang::sym

#' @export
rlang::syms

#' @export
rlang::UQ



# rmarkdown ====
#' @importFrom rmarkdown render
#' @export
rmarkdown::render



# stringr ====
#' @import stringr
#' @export
stringr::str_detect

#' @export
stringr::str_extract

#' @export
stringr::str_extract_all

#' @export
stringr::str_match

#' @export
stringr::str_match_all

#' @export
stringr::str_pad

#' @export
stringr::str_replace

#' @export
stringr::str_replace_all

#' @export
stringr::str_split

#' @export
stringr::str_subset



## tibble ====
#' @importFrom tibble as_tibble glimpse is_tibble remove_rownames
#'   rownames_to_column tibble
#' @export
tibble::as_tibble

#' @export
tibble::glimpse

#' @export
tibble::is_tibble

#' @export
tibble::remove_rownames

#' @export
tibble::rownames_to_column

#' @export
tibble::tibble



# tidyr ====
#' @importFrom tidyr expand_ nest_ separate_ unnest_
#' @export
tidyr::expand_

#' @export
tidyr::nest_

#' @export
tidyr::separate_

#' @export
tidyr::unnest_



## tools ====
#' @importFrom tools file_path_sans_ext
#' @export
tools::file_path_sans_ext



# utils ====
#' @importFrom utils download.file globalVariables sessionInfo
#' @export
utils::download.file

#' @export
utils::globalVariables

#' @export
utils::sessionInfo
