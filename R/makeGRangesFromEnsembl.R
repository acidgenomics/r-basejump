#' Make `GRanges` from Ensembl
#'
#' @name makeGRanges
#' @inheritParams params
#'
#' @section Broad class definitions:
#'
#' For gene and transcript annotations, a `broadClass` column is added, which
#' generalizes the gene types into a smaller number of semantically-meaningful
#' groups:
#'
#'   - `coding`.
#'   - `noncoding`.
#'   - `pseudo`.
#'   - `small`.
#'   - `decaying`.
#'   - `ig` (immunoglobulin).
#'   - `tcr` (T cell receptor).
#'   - `other`.
#'
#' @section GRCh37 (hg19) legacy annotations:
#'
#' [makeGRangesFromEnsembl()] supports the legacy *Homo sapiens* GRCh37 (release
#' 75) build by internally querying the [EnsDb.Hsapiens.v75][] package.
#' Alternatively, the corresponding GTF/GFF file can be loaded directly from
#' GENCODE or Ensembl.
#'
#' [EnsDb.Hsapiens.v75]: https://bioconductor.org/packages/EnsDb.Hsapiens.v75/
#'
#' @param release `integer(1)`.
#'   Ensembl release version (e.g. `90`). If set `NULL`, defaults to the most
#'   recent release available.
#'
#' @return `GRanges`.
#'
#' @seealso
#' - [AnnotationHub](https://bioconductor.org/packages/AnnotationHub/).
#' - [ensembldb](https://bioconductor.org/packages/ensembldb/).
#'
#' @examples
#' ## makeGRangesFromEnsembl ====
#' ## Genes
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "genes")
#' summary(x)
#'
#' ## Transcripts
#' x <- makeGRangesFromEnsembl("Homo sapiens", level = "transcripts")
#' summary(x)
#'
#' ## makeGRangesFromEnsDb ====
#' x <- makeGRangesFromEnsDb("EnsDb.Hsapiens.v75")
NULL



#' @describeIn makeGRanges
#' Quickly obtain gene and transcript annotations from
#' [Ensembl](https://www.ensembl.org/) using
#' [AnnotationHub](https://bioconductor.org/packages/AnnotationHub/) and
#' [ensembldb](https://bioconductor.org/packages/ensembldb/).
#'
#' Simply specify the desired organism, using the full latin name. For example,
#' we can obtain human annotations with `Homo sapiens`. Optionally, specific
#' Ensembl genome builds (e.g. `GRCh38`) and release versions (e.g. `87`) are
#' supported.
#'
#' Under the hood, this function fetches annotations from AnnotationHub using
#' the ensembldb package. AnnotationHub supports versioned Ensembl releases,
#' back to version 87.
#'
#' Genome build: use "`GRCh38`" instead of "`hg38`" for the genome build, since
#' we're querying Ensembl and not UCSC.
#' @export
makeGRangesFromEnsembl <- function(
    organism,
    level = c("genes", "transcripts"),
    genomeBuild = NULL,
    release = NULL
) {
    message("Making GRanges from Ensembl.")
    assert(isString(organism))
    level <- match.arg(level)
    if (
        identical(tolower(organism), "homo sapiens") &&
        (
            identical(tolower(as.character(genomeBuild)), "grch37") ||
            identical(release, 75L)
        )
    ) {
        id <- "EnsDb.Hsapiens.v75"
        edb <- .getEnsDbFromPackage(package = id)
    } else {
        id <- .getAnnotationHubID(
            organism = organism,
            genomeBuild = genomeBuild,
            ensemblRelease = release
        )
        edb <- .getEnsDbFromAnnotationHub(id = id)
    }
    gr <- makeGRangesFromEnsDb(object = edb, level = level)
    metadata(gr)[["id"]] <- id
    gr
}



#' @describeIn makeGRanges
#' [annotable()] is a legacy convenience function that calls
#' [makeGRangesFromEnsembl()] and returns a `tibble` instead of `GRanges`. Note
#' that `GRanges` can also be coercing using
#' [`as.data.frame()`][BiocGenerics::as.data.frame].
#' @export
annotable <-
    function() {
        gr <- do.call(
            what = makeGRangesFromEnsembl,
            args = matchArgsToDoCall()
        )
        # Decode run-length encoding in mcols before coercing to tibble.
        # Otherwise Windows users won't get expected atomic columns.
        mcols(gr) <- decode(mcols(gr))
        as_tibble(gr, rownames = NULL)
    }

formals(annotable) <- formals(makeGRangesFromEnsembl)
