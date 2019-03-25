# FIXME Import code from freerange
# @inheritParams makeGRangesFromEnsembl



#' Make a Gene2Symbol object
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeGene2Symbol
#' @inheritParams params
#'
#' @return `Gene2Symbol`.
#'
#' @examples
#' ## makeGene2SymbolFromEnsembl ====
#' x <- makeGene2SymbolFromEnsembl(organism = "Homo sapiens")
#' print(x)
#'
#' ## makeTx2GeneFromEnsDb ====
#' x <- makeGene2SymbolFromEnsDb("EnsDb.Hsapiens.v75")
#' print(x)
#'
#' ## makeGene2SymbolFromGFF ====
#' ## GTF
#' file <- file.path(basejumpCacheURL, "example.gtf")
#' x <- makeGene2SymbolFromGFF(file)
#' print(x)
#'
#' ## GFF3
#' file <- file.path(basejumpCacheURL, "example.gff3")
#' x <- makeGene2SymbolFromGFF(file)
#' print(x)
NULL



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromEnsembl <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(args = list(level = "genes"))
        )
        Gene2Symbol(gr)
    }

# FIXME Update this once freerange is added.
# f <- formals(makeGRangesFromEnsembl)
# f <- f[setdiff(names(f), "level")]
# formals(makeGene2SymbolFromEnsembl) <- f



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromEnsDb <- function(object) {
    gr <- makeGRangesFromEnsDb(object)
    Gene2Symbol(gr)
}



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromGFF <- function(file) {
    gr <- makeGRangesFromGFF(file, level = "genes")
    Gene2Symbol(gr)
}



#' @rdname makeGene2Symbol
#' @usage NULL
#' @export
makeGene2SymbolFromGTF <- makeGene2SymbolFromGFF
