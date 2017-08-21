# cowplot ====
#' @export
cowplot::draw_plot

#' @export
cowplot::ggdraw

#' @export
cowplot::plot_grid



# datasets ====
#' @inherit datasets::mtcars
#' @keywords internal
#' @return [data.frame].
#' @export
datasets::mtcars -> mtcars



# dplyr ====
#' @export
dplyr::arrange

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

#' @rdname AllGenerics
#' @usage NULL
#' @export
dplyr::filter -> tidy_filter  # nolint

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
dplyr::mutate

#' @export
dplyr::mutate_all

#' @export
dplyr::mutate_if

#' @export
dplyr::n

#' @export
dplyr::pull

#' @export
dplyr::right_join

#' @rdname AllGenerics
#' @usage NULL
#' @export
dplyr::select -> tidy_select  # nolint

#' @inherit dplyr::starwars
#' @keywords internal
#' @return [tibble].
#' @export
dplyr::starwars -> starwars

#' @export
dplyr::summarize

#' @export
dplyr::summarize_all

#' @export
dplyr::summarize_if

#' @export
dplyr::top_n

#' @export
dplyr::ungroup



# graphics ====
#' @export
graphics::hist



# httr ====
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
magrittr::`%$%`

#' @export
magrittr::`%>%`

#' @export
magrittr::set_colnames

#' @export
magrittr::set_rownames



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
#' @export
rlang::`!!!`

#' @export
rlang::`!!`

#' @export
rlang::.data

#' @export
rlang::eval_bare

#' @export
rlang::eval_tidy

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

#' @export
rlang::sym

#' @export
rlang::syms

#' @export
rlang::UQ



# rmarkdown ====
#' @export
rmarkdown::render



# rmarkdown ====
#' @export
rmarkdown::render



# stats ====
#' @export
stats::formula

#' @export
stats::hclust

#' @export
stats::setNames



# stringr ====
#' @export
stringr::fixed

#' @export
stringr::regex

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

#' @export
stringr::str_trunc



# S4Vectors ====
#' @export
S4Vectors::DataFrame

#' @export
S4Vectors::metadata

#' @export
S4Vectors::SimpleList



# tibble ====
#' @export
tibble::column_to_rownames

#' @export
tibble::glimpse

#' @export
tibble::has_rownames

#' @export
tibble::is_tibble

#' @export
tibble::remove_rownames

#' @export
tibble::rownames_to_column

#' @export
tibble::tibble



# tidyr ====
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
utils::object.size

#' @export
utils::packageVersion

#' @export
utils::read.delim

#' @export
utils::sessionInfo
