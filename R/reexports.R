# AnnotationDbi ====
#' @export
AnnotationDbi::select



# Biobase ====
#' @export
Biobase::content



# BiocGenerics ====
#' @export
BiocGenerics::intersect



# BiocInstaller ====
#' @export
BiocInstaller::biocValid



# BiocStyle ====
#' @export
BiocStyle::doc_date

#' @export
BiocStyle::html_document
# [fix] rmarkdown::html_document instead?

#' @export
BiocStyle::html_document2

#' @export
BiocStyle::pdf_document

#' @export
BiocStyle::pdf_document2



# biomaRt ====
#' @export
biomaRt::getBM

#' @export
biomaRt::listMarts

#' @export
biomaRt::useEnsembl

#' @export
biomaRt::useMart



# devtools ====
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



# dplyr ====
#' @export
dplyr::bind_cols

#' @export
dplyr::bind_rows

#' @export
dplyr::case_when

#' @export
dplyr::desc

#' @export
dplyr::distinct

#' @export
dplyr::everything

#' @export
dplyr::full_join

#' @export
dplyr::funs

#' @export
dplyr::group_by

#' @export
dplyr::inner_join

#' @export
dplyr::left_join

#' @export
dplyr::mutate_all

#' @export
dplyr::n

#' @export
dplyr::pull

#' @export
dplyr::right_join

#' @rdname tidy
#' @usage NULL
#' @export
dplyr::select -> tidy_select

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
#' @export
graphics::hist



# httr ====
# Collison with Biobase::content
#' @export
httr::content_type_json

#' @export
httr::GET

#' @export
httr::user_agent



# knitr ====
#' @export
knitr::asis_output

#' @export
knitr::kable

#' @export
knitr::knit

#' @export
knitr::opts_chunk

#' @export
knitr::opts_knit



# magrittr ====
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



# parallel ====
#' @export
parallel::mclapply

#' @export
parallel::mcmapply



# pbapply ====
#' @export
pbapply::pblapply

#' @export
pbapply::pbsapply



# pbmcapply ====
#' @export
pbmcapply::pbmclapply



# R.utils ====
#' @export
R.utils::gunzip

#' @export
R.utils::gzip



# RCurl ====
#' @export
RCurl::getURL



# readr ====
#' @export
readr::cols

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
#' @export
readxl::read_excel



# reshape ====
#' @export
reshape2::melt



# rlang ====
# Collison with Biobase::exprs
#' @export
rlang::`!!!`

#' @export
rlang::`!!`

#' @export
rlang::.data

#' @export
rlang::is_atomic

#' @export
rlang::is_bytes

#' @export
rlang::is_character

#' @export
rlang::is_double

#' @export
rlang::is_integer

#' @export
rlang::is_list

#' @export
rlang::is_logical

#' @export
rlang::is_null

#' @export
rlang::is_raw

#' @export
rlang::is_string

#' @export
rlang::is_vector

#' @export
rlang::quo

#' @export
rlang::quos

#' @export
rlang::set_names

# Don't export over [stats::setNames]
rlang::set_names -> setNames

#' @export
rlang::sym

#' @export
rlang::syms

#' @export
rlang::UQ



# rmarkdown ====
#' @export
rmarkdown::render



# stringr ====
#' @export
stringr::str_c

#' @export
stringr::str_detect

#' @export
stringr::str_extract

#' @export
stringr::str_extract_all

#' @export
stringr::str_length

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
stringr::str_sub

#' @export
stringr::str_subset



# S4Vectors ====
#' @export
S4Vectors::expand

#' @export
S4Vectors::first

#' @export
S4Vectors::rename



# tibble ====
# Collison with rlang on full import
#' @export
tibble::as_tibble

#' @export
tibble::column_to_rownames

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
# Collison with S4Vectors::expand
#' @export
tidyr::expand_

#' @export
tidyr::nest

#' @export
tidyr::nest_

#' @export
tidyr::separate_

#' @export
tidyr::unnest

#' @export
tidyr::unnest_



# tools ====
#' @export
tools::file_path_sans_ext



# utils ====
#' @export
utils::download.file

#' @export
utils::globalVariables

#' @export
utils::sessionInfo
