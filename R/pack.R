#' Build package and website
#' @export
#' @importFrom devtools build build_vignettes check document load_all
#' @importFrom pkgdown build_site
pack <- function() {
    devtools::load_all()
    devtools::document()
    devtools::build_vignettes()
    devtools::check()
    devtools::build()
    pkgdown::build_site()
}
