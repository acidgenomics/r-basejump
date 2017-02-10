#' Build package and website
#' @export
#' @importFrom devtools build check document
#' @importFrom pkgdown build_site
buildAll <- function() {
    devtools::document()
    devtools::build_vignettes()
    devtools::check()
    devtools::build()
    pkgdown::build_site()
}
