#' Make a Tx2Gene object from transcriptome FASTA
#'
#' @export
#' @note Updated 2019-10-24.
#'
#' @inheritParams acidroxygen::params
#' @param source `character(1)`.
#'   FASTA file source:
#'
#'   - `"ensembl"`: Ensembl.
#'   - `"gencode"`: GENCODE.
#'   - `"refseq"`: RefSeq.
#'   - `"flybase"`: FlyBase.
#'   - `"wormbase"`: WormBase.
#'
#'   Assuming Ensembl transcriptome (i.e. cDNA) input by default.

#' @seealso
#' tx2gene importers defined in [koopa][].
#' [koopa]: https://koopa.acidgenomics.com/
#'
#' @examples
#' file <- pasteURL(
#'     "ftp.ensembl.org",
#'     "pub",
#'     "release-98",
#'     "fasta",
#'     "homo_sapiens",
#'     "cdna",
#'     "Homo_sapiens.GRCh38.cdna.all.fa.gz",
#'     protocol = "ftp"
#' )
#' makeTx2GeneFromFASTA(file, source = "ensembl")
makeTx2GeneFromFASTA <- function(
    file,
    source = c(
        "ensembl",
        "gencode",
        "refseq",
        "flybase",
        "wormbase"
    )
) {
    x <- import(file, format = "lines")
    source <- match.arg(source)
    ## FIXME Remove once we add support for the other importers.
    stopifnot(identical(source, "ensembl"))

    ## Ensembl -----------------------------------------------------------------
    x <- grep(pattern = "^>", x = x, value = TRUE)
    ## Strip the ">" prefix.
    x <- substr(x, start = 2L, stop = nchar(x))
    x <- strsplit(x = x, split = " ", fixed = TRUE)
    x <- lapply(
        X = x,
        FUN = function(x) {
            x[c(1L, 4L)]
        }
    )
    x <- do.call(what = rbind, args = x)
    x[, 2L] <- gsub(pattern = "^gene:", replacement = "", x = x[, 2L])



    x <- unique(x)
    Tx2Gene(x)
}
