# dplyr ====
#' @rdname AllGenerics
#' @usage NULL
#' @export
dplyr::filter -> tidy_filter  # nolint

#' @rdname AllGenerics
#' @usage NULL
#' @export
dplyr::select -> tidy_select  # nolint



# magrittr ====
#' @export
magrittr::`%>%`

#' @export
magrittr::set_colnames

#' @export
magrittr::set_rownames



# Matrix ====
#' @export
Matrix::Matrix



# readr ====
#' @export
readr::read_csv

#' @export
readr::read_tsv

#' @export
readr::write_csv

#' @export
readr::write_tsv



# S4Vectors ====
#' @export
S4Vectors::DataFrame

#' @export
S4Vectors::metadata



# tibble ====
#' @export
tibble::column_to_rownames

#' @export
tibble::glimpse

#' @export
tibble::is_tibble

#' @export
tibble::rownames_to_column

#' @export
tibble::tibble
