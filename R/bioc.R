# Modified version of `rafalib::install_bioc()`
# https://github.com/rafalab/rafalib/blob/master/R/install_bioc.R



#' @keywords internal
globalVariables("biocLite")



#' Install or update Bioconductor packages
#'
#' This is function simply a wrapper for \code{biocLite}. It first sources the
#' code from the Bioconductor website then calls \code{biocLite}.
#'
#' @author Michael Steinbaugh
#'
#' @keywords internal
#'
#' @param ... Passthrough to \code{biocLite()}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load Bioconductor and update packages
#' bioc()
#'
#' # Install `limma`
#' bioc("limma")
#' }
bioc <- function(...) {
    internet <- try(source("http://bioconductor.org/biocLite.R"), silent = TRUE)
    if (!class(internet) == "try-error") {
        biocLite(...)
    } else {
        stop("No connection to http://bioconductor.org.")
    }
}
