#' Genome versions
#'
#' Obtain the latest release version from various genome annotation sources.
#'
#' @name genomeVersions
#' @note Updated 2019-10-18.
#'
#' @inheritParams acidroxygen::params
#' @param dmel `logical(1)`.
#'   Return *Drosophila melanogaster* genome release version.
#'
#' @return `character(1)`.
#'   Release version.
#'
#' @seealso
#' Refer to the koopa package for shell variants:
#'
#' - `ensembl-version`.
#' - `flybase-version`.
#' - `gencode-version`.
#' - `refseq-version`.
#' - `wormbase-version`.
#'
#' @examples
#' if (hasInternet()) {
#'     ensemblVersion()
#'     flybaseVersion()
#'     gencodeVersion(organism = "Homo sapiens")
#'     refseqVersion()
#'     wormbaseVersion()
#' }
NULL



#' @rdname genomeVersions
#' @export
ensemblVersion <- function() {
    suppressMessages(
        x <- import(
            file = "ftp://ftp.ensembl.org/pub/current_README",
            format = "lines"
        )
    )
    x <- x[[3L]]
    x <- str_split_fixed(x, pattern = boundary("word"), n = 4L)[1L, 3L]
    x
}



#' @importFrom RCurl getURL



#' @rdname genomeVersions
#' @export
flybaseVersion <- function(dmel = FALSE) {
    assert(isFlag(dmel))
    x <- getURL(
        url = "ftp://ftp.flybase.net/releases/",
        dirlistonly = TRUE
    )
    x <- unlist(strsplit(x, split = "\n"))
    x <- grep(pattern = "^FB[0-9]{4}_[0-9]{2}$", x = x, value = TRUE)
    x <- sort(x)
    x <- tail(x, n = 1L)
    x


    ## dmel mode
    ## "ftp://ftp.flybase.net/releases/current/"
}



#' @rdname genomeVersions
#' @export
gencodeVersion <- function(organism) {
    organism <- match.arg(
        arg = organism,
        choices = c("Homo sapiens", "Mus musculus")
    )
}



#' @rdname genomeVersions
#' @export
refseqVersion <- function() {
}



#' @rdname genomeVersions
#' @export
wormbaseVersion <- function() {
}
