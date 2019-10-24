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
#'   - `"flybase"`: FlyBase.
#'
#'   Assuming Ensembl transcriptome (i.e. cDNA) input by default.

#' @seealso
#' tx2gene importers defined in [koopa][].
#' [koopa]: https://koopa.acidgenomics.com/
#'
#' @examples
#' ## Ensembl ====
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
#'
#' ## GENCODE ====
#' file <- pasteURL(
#'     "ftp.ebi.ac.uk",
#'     "pub",
#'     "databases",
#'     "gencode",
#'     "Gencode_human",
#'     "release_32",
#'     "gencode.v32.transcripts.fa.gz",
#'     protocol = "ftp"
#' )
#' makeTx2GeneFromFASTA(file, source = "gencode")
makeTx2GeneFromFASTA <- function(
    file,
    source = c(
        ## "refseq"
        ## "wormbase"
        "ensembl",
        "gencode",
        "flybase"
    )
) {
    x <- import(file, format = "lines")
    source <- match.arg(source)
    x <- grep(pattern = "^>", x = x, value = TRUE)
    ## Error if the input doesn't contain ">". For example, this happens with
    ## bcbio "ref-transcripts.fa" file returned by gffutils.
    assert(hasLength(x))
    x <- substr(x, start = 2L, stop = nchar(x))
    if (identical(source, "ensembl")) {
        ## Ensembl -------------------------------------------------------------
        x <- strsplit(x = x, split = " ", fixed = TRUE)
        x <- lapply(
            X = x,
            FUN = function(x) {
                x[c(1L, 4L)]
            }
        )
        x <- do.call(what = rbind, args = x)
        x[, 2L] <- gsub(pattern = "^gene:", replacement = "", x = x[, 2L])
    } else if (identical(source, "gencode")) {
        ## GENCODE -------------------------------------------------------------
        x <- strsplit(x = x, split = "|", fixed = TRUE)
        x <- lapply(
            X = x,
            FUN = function(x) {
                x[c(1L, 2L)]
            }
        )
        x <- do.call(what = rbind, args = x)
    } else if (identical(source, "flybase")) {
        ## FlyBase -------------------------------------------------------------
        x <- strsplit(x = x, split = " ", fixed = TRUE)
        x <- lapply(
            X = x,
            FUN = function(x) {
                x[c(1L, 9L)]
            }
        )
        x <- do.call(what = rbind, args = x)
        x[, 2L] <- gsub(
            pattern = "^.*\\b(FBgn[0-9]{7})\\b.*$",
            replacement = "\\1",
            x = x[, 2L]
        )
    }
    x <- unique(x)
    Tx2Gene(x)
}
