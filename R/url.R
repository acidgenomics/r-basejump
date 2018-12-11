# TODO Add query string "?" with "&" handling.
# TODO Consider adding "!/#" support?

#' Concatenate Strings to Form a URL
#'
#' @inheritParams base::paste
#' @export
#'
#' @param protocol `character(1)`. Desired protocol to use. Defaults to HTTPS
#'   but HTTP and FTP are also supported. Use `"none"` if you want to prepare
#'   a URL that already contains a protocol in the first element of the dots.
#'
#' @examples
#' ## HTTPS
#' x <- url(
#'     "steinbaugh.com",
#'     "basejump",
#'     "reference",
#'     protocol = "https"
#' )
#' print(x)
#'
#' ## FTP
#' x <- url(
#'     "ftp.ensembl.org",
#'     "pub",
#'     "release-94",
#'     "gtf",
#'     "homo_sapiens",
#'     "Homo_sapiens.GRCh38.94.gtf.gz",
#'     protocol = "ftp"
#' )
#' print(x)
url <- function(..., protocol = c("https", "http", "ftp", "none")) {
    protocol <- match.arg(protocol)
    url <- paste(..., sep = "/")
    assert(isString(url))
    if (protocol != "none") {
        url <- paste0(protocol, "://", url)
    }
    assert(containsAURL(url))
    url
}
