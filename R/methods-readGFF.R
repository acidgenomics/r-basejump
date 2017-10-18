#' Read GFF/GTF Annotations
#'
#' @rdname readGFF
#' @name readGFF
#' @family Data Import and Project Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams saveData
#'
#' @details The GFF (General Feature Format) format consists of one line per
#'   feature, each containing 9 columns of data, plus optional track definition
#'   lines. The GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @seealso
#' - http://www.ensembl.org/info/website/upload/gff.html
#' - http://www.gencodegenes.org/gencodeformat.html
#'
#' @return [data.frame].
#'
#' @examples
#' file.path(testDataURL, "mmusculus.gtf") %>%
#'     readGFF() %>%
#'     str()
NULL



# Constructors ====
#' @importFrom utils read.delim
.readGFF <- function(
    object,
    quiet = FALSE) {
    file <- .localOrRemoteFile(object, quiet = quiet)
    if (!isTRUE(quiet)) {
        message(paste("Reading GFF:", names(file)))
    }
    gff <- tryCatch(
        read.delim(
            file,
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
            stop("GFF file failed to load", call. = FALSE)
        },
        warning = function(w) {
            stop("GFF file failed to load", call. = FALSE)
        })
    gff
}



# Methods ====
#' @rdname readGFF
#' @export
setMethod(
    "readGFF",
    signature("character"),
    .readGFF)
