globalVariables("biocLite")

#' @importFrom utils installed.packages
.biocLite <- function(pkgs, ...) {
    assert_is_character(pkgs)
    install <- setdiff(pkgs, installed.packages()[, "Package"])
    if (length(install)) {
        internet <- try(
            expr = source("http://bioconductor.org/biocLite.R"),
            silent = TRUE
        )
        if (class(internet) == "try-error") {
            abort("Connection to http://bioconductor.org failed")
        } else {
            biocLite(pkgs, ...)
        }
    }
    require(pkgs, character.only = TRUE)
}
