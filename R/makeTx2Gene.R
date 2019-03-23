#' Make a Tx2Gene object
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeTx2Gene
#' @include makeGRangesFromEnsembl.R
#' @inheritParams makeGRangesFromEnsembl
#' @inheritParams params
#'
#' @seealso `makeGRanges`.
#'
#' @return `Tx2Gene`.
#'
#' @examples
#' ## makeTx2GeneFromEnsembl ====
#' x <- makeTx2GeneFromEnsembl(organism = "Homo sapiens")
#' print(x)
#'
#' ## makeTx2GeneFromEnsDb ====
#' x <- makeTx2GeneFromEnsDb("EnsDb.Hsapiens.v75")
#' print(x)
#'
#' ## makeTx2GeneFromGFF ====
#' ## GTF
#' file <- file.path(basejumpCacheURL, "example.gtf")
#' x <- makeTx2GeneFromGFF(file)
#' print(x)
#'
#' ## GFF3
#' file <- file.path(basejumpCacheURL, "example.gff3")
#' x <- makeTx2GeneFromGFF(file)
#' print(x)
NULL



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromEnsembl <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(args = list(level = "transcripts"))
        )
        Tx2Gene(gr)
    }

f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), c("level", "metadata", "..."))]
formals(makeTx2GeneFromEnsembl) <- f



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromEnsDb <- function(object) {
    gr <- makeGRangesFromEnsDb(object, level = "transcripts")
    Tx2Gene(gr)
}



#' @rdname makeTx2Gene
#' @export
makeTx2GeneFromGFF <- function(file) {
    gr <- makeGRangesFromGFF(file, level = "transcripts")
    Tx2Gene(gr)
}



#' @rdname makeTx2Gene
#' @usage NULL
#' @export
makeTx2GeneFromGTF <- makeTx2GeneFromGFF
