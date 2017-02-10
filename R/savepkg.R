savepkg <- function() {
    devtools::document()
    devtools::check()
    pkgdown::build_site()
}
