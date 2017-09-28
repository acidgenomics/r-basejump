#' Read GFF Annotations
#'
#' Read GFF (General Feature Format) file.
#'
#' @rdname readGFF
#' @name readGFF
#'
#' @seealso
#' - http://www.ensembl.org/info/website/upload/gff.html
#' - http://www.gencodegenes.org/gencodeformat.html
#'
#' @return [data.frame].
#'
#' @examples
#' # GTF is GFF-compatible
#' file.path(testDataURL, "mmusculus.gtf") %>%
#'     readGFF() %>%
#'     str()
NULL



# Constructors ====
.readGFF <- function(object) {
    file <- .localOrRemoteFile(object)
    message(paste("Reading GFF:", names(file)))
    gff <- tryCatch(
        read.delim(
            file,
            col.names = c("chromosome",
                          "annotationSource",
                          "featureType",
                          "start",
                          "end",
                          "score",
                          "strand",
                          "phase",
                          "keyValuePairs"),
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
setMethod("readGFF", "character", .readGFF)



# Aliases ====
#' @rdname readGFF
#' @export
readGTF <- readGFF
