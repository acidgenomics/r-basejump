#' Make a Gene2Symbol object
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeGene2Symbol
#'
#' @inheritParams params
#' @inheritParams Gene2Symbol
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
#' file <- file.path(basejumpTestsURL, "example.gtf")
#' x <- makeGene2SymbolFromGFF(file)
#' print(x)
#'
#' ## GFF3
#' file <- file.path(basejumpTestsURL, "example.gff3")
#' x <- makeGene2SymbolFromGFF(file)
#' print(x)
NULL



#' @rdname makeGene2Symbol
#' @export
## Updated 2019-06-06.
makeGene2SymbolFromEnsembl <-
    function(organism) {
        format <- match.arg(format)
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall(
                args = list(level = "genes"),
                removeFormals = "format"
            )
        )
        Gene2Symbol(gr, format = format)
    }

f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), "level")]
f[["format"]] <- formals(Gene2Symbol.DataFrame)[["format"]]
formals(makeGene2SymbolFromEnsembl) <- f



#' @rdname makeGene2Symbol
#' @export
## Updated 2019-06-06.
makeGene2SymbolFromEnsDb <-
    function(object, format) {
        format <- match.arg(format)
        gr <- makeGRangesFromEnsDb(object)
        Gene2Symbol(gr, format = format)
    }

formals(makeGene2SymbolFromEnsDb)[["format"]] <-
    formals(makeGene2SymbolFromEnsembl)[["format"]]



#' @rdname makeGene2Symbol
#' @export
## Updated 2019-06-06.
makeGene2SymbolFromGFF <-
    function(file, format) {
        format <- match.arg(format)
        gr <- makeGRangesFromGFF(file, level = "genes")
        Gene2Symbol(gr, format = format)
    }

formals(makeGene2SymbolFromGFF)[["format"]] <-
    formals(makeGene2SymbolFromEnsembl)[["format"]]



#' @rdname makeGene2Symbol
#' @usage NULL
#' @export
makeGene2SymbolFromGTF <- makeGene2SymbolFromGFF
