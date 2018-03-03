#' Read GFF/GTF Annotations
#'
#' @family Read Functions
#'
#' @importFrom utils read.delim
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
#' @export
#'
#' @examples
#' readGFF("http://basejump.seq.cloud/mmusculus.gtf") %>% glimpse()
readGFF <- function(object) {
    assert_is_a_string(object)
    assert_all_are_matching_regex(object, "\\.g(f|t)f(\\d)?$")
    file <- localOrRemoteFile(object)
    inform(paste("Reading GFF/GTF:", names(file)))
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
            header = FALSE
        ),
        error = function(e) {
            abort("GFF/GTF file failed to load")
        },
        warning = function(w) {
            abort("GFF/GTF file failed to load")
        }
    )
    gff
}



# Aliases ======================================================================
#' @rdname readGFF
#' @export
readGFF -> readGTF
