#' Make Gene-to-Symbol Mappings
#'
#' @section GFF/GTF file:
#'
#' Remote URLs and compressed files are supported.
#'
#' @name makeGene2Symbol
#' @include makeGRanges.R
#'
#' @inheritParams makeGRanges
#'
#' @seealso [makeGRanges].
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



# Ensembl ======================================================================
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
f <- formals(makeGRangesFromEnsembl)
f <- f[setdiff(names(f), "level")]
formals(makeGene2SymbolFromEnsembl) <- f



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromEnsDb <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsDb,
            args = matchArgsToDoCall(args = list(level = "genes"))
        )
        Gene2Symbol(gr)
    }
f <- formals(makeGRangesFromEnsDb)
f <- f[setdiff(names(f), "level")]
formals(makeGene2SymbolFromEnsDb) <- f



# GTF/GFF ======================================================================
.genomeMetadataFromGFF <- function(object) {
    assert_is_all_of(object, "DataFrame")
    organism <- tryCatch(
        expr = detectOrganism(object[["geneID"]]),
        error = function(e) character()
    )
    metadata(object) <- list(
        organism = organism,
        genomeBuild = character(),
        ensemblRelease = integer()
    )
    object
}



#' @rdname makeGene2Symbol
#' @export
makeGene2SymbolFromGFF <- function(file) {
    message("Making Gene2Symbol from GFF.")
    gff <- import(file)

    source <- .gffSource(gff)
    type <- .gffType(gff)
    message(paste(source, type, "detected."))

    # Coerce to tibble.
    data <- camel(as_tibble(gff))

    # Require `geneID` column.
    assert_is_subset("geneID", colnames(data))

    # Filter rows that don't contain gene annotations.
    data <- filter(data, !is.na(!!sym("geneID")))

    if (type == "GTF") {
        if (
            !"geneName" %in% colnames(data) &&
            "geneSymbol" %in% colnames(data)
        ) {
            # Needed for FlyBase.
            data[["geneName"]] <- data[["geneSymbol"]]  # nocov
        }
    } else if (type == "GFF") {
        if (
            !"geneName" %in% colnames(data) &&
            "name" %in% colnames(data)
        ) {
            data[["geneName"]] <- data[["name"]]
        }
    }

    data <- as(data, "DataFrame")
    metadata(data) <- .genomeMetadataFromGFF(data)
    Gene2Symbol(data)
}



# Aliases ======================================================================
#' @rdname makeGene2Symbol
#' @usage NULL
#' @export
makeGene2SymbolFromGTF <- makeGene2SymbolFromGFF
