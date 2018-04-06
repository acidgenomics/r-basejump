# Only reexport functions needed for working examples and vignette

#' @importFrom Matrix Matrix
#' @export
Matrix::Matrix

#' @importFrom S4Vectors DataFrame
#' @export
S4Vectors::DataFrame

#' @importFrom S4Vectors mcols
#' @export
S4Vectors::mcols

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr set_rownames
#' @export
magrittr::set_rownames

#' @importFrom tibble glimpse
#' @export
tibble::glimpse

#' @importFrom tibble is_tibble
#' @export
tibble::is_tibble

#' @importFrom tibble tibble
#' @export
tibble::tibble
