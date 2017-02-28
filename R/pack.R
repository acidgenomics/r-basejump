#' Build package and website
#' @export
#' @import BiocCheck
#' @importFrom devtools build build_vignettes check document install load_all
#' @importFrom pkgdown build_site
pack <- function() {
    devtools::load_all()
    devtools::document()
    devtools::build_vignettes()
    BiocCheck::BiocCheck(getwd())
    devtools::check()
    devtools::build()
    devtools::install()
    pkgdown::build_site()
}
