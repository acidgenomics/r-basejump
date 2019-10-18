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
#' ensemblVersion()
NULL



#' @rdname genomeVersions
#' @export
ensemblVersion <- function() {
    x <- import(
        file = "ftp://ftp.ensembl.org/pub/current_README",
        format = "lines"
    )
    x <- x[[3L]]
    x <- str_split_fixed(x, pattern = boundary("word"), n = 4L)[1L, 3L]
    x
}



#' @rdname genomeVersions
#' @export
flybaseVersion <- function(dmel = FALSE) {
    assert(isFlag(dmel))
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
