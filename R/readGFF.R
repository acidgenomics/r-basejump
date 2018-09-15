#' Read GFF Annotations
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' Column names follow the
#' [conventions defined by Ensembl](https://bit.ly/2K6EBla).
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `GRanges`.
#'
#' @seealso
#' - [Ensembl](http://www.ensembl.org/info/website/upload/gff.html)
#' - [Gencode](http://www.gencodegenes.org/gencodeformat.html)
#'
#' @examples
#' x <- readGFF("http://basejump.seq.cloud/example.gtf")
#' summary(x)
readGFF <- function(file) {
    assert_is_a_string(file)
    assert_all_are_matching_regex(file, "\\.g(f|t)f(\\d)?(\\.gz)?$")

    message(paste("Reading", basename(file)))
    file <- localOrRemoteFile(file)

    gff <- tryCatch(
        rtracklayer::import(file),
        error = function(e) {
            stop("GFF file failed to load")  # nocov
        },
        warning = function(w) {
            stop("GFF file failed to load")  # nocov
        }
    )

    gff
}



# Aliases ======================================================================
#' @rdname readGFF
#' @export
readGTF <- readGFF
