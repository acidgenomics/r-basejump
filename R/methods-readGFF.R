#' Read GFF/GTF Annotations
#'
#' @rdname readGFF
#' @name readGFF
#' @family Data Import and Project Utilities
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @details The GFF (General Feature Format) format consists of one line per
#'   feature, each containing 9 columns of data, plus optional track definition
#'   lines. The GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @seealso
#' - [Ensembl](http://www.ensembl.org/info/website/upload/gff.html)
#' - [Gencode](http://www.gencodegenes.org/gencodeformat.html)
#'
#' @return [data.frame].
#'
#' @examples
#' readGFF("http://basejump.seq.cloud/mmusculus.gtf") %>% glimpse()
NULL



# Constructors =================================================================
#' @importFrom utils read.delim
.readGFF <- function(
    object,
    quiet = FALSE) {
    assert_is_a_string(object)
    assert_is_a_bool(quiet)
    file <- localOrRemoteFile(object, quiet = quiet)
    if (!isTRUE(quiet)) {
        inform(paste("Reading GFF/GTF:", names(file)))
    }
    gff <- tryCatch(
        read.delim(
            file = file,
            col.names = c(
                "chromosome",
                "annotationSource",
                "featureType",
                "start",
                "end",
                "score",
                "strand",
                "phase",
                "keyValuePairs"
            ),
            comment.char = "#",
            header = FALSE),
        error = function(e) {
            abort("GFF/GTF file failed to load")
        },
        warning = function(w) {
            abort("GFF/GTF file failed to load")
        })
    gff
}



# Methods ======================================================================
#' @rdname readGFF
#' @export
setMethod(
    "readGFF",
    signature("character"),
    .readGFF)



# Aliases ======================================================================
#' @rdname readGFF
#' @inheritParams general
#' @export
readGTF <- function(...) {
    readGFF(...)
}
