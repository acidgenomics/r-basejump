#' Get remote URL directory listing
#'
#' @export
#' @note Best served using FTP instead of HTTP.
#' @note Updated 2020-01-18.
#'
#' @inheritParams acidroxygen::params
#'
#' @return `character`.
#'   Simple directory contents return, including both files and subdirectories.
#'
#' @examples
#' if (
#'     goalie::hasInternet() &&
#'     !isTRUE(nzchar(Sys.getenv("CI")))
#' ) {
#'     url <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/Homo_sapiens/current"
#'     x <- getURLDirList(url)
#'     tail(x)
#' }
getURLDirList <- function(url, pattern = NULL) {
    assert(
        isAURL(url),
        isString(pattern, nullOK = TRUE)
    )
    if (!isTRUE(grepl("/$", url))) {
        url <- paste0(url, "/")
    }
    x <- getURL(url = url, dirlistonly = TRUE)
    x <- unlist(strsplit(x, split = "\n"))
    if (isString(pattern)) {
        keep <- grepl(pattern = pattern, x = x)
        x <- x[keep]
    }
    x
}
