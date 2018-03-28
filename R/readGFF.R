gffCols <- c(
    "chromosome",
    "annotationSource",
    "featureType",
    "start",
    "end",
    "score",
    "strand",
    "phase",
    "keyValuePairs"
)



#' Read GFF/GTF Annotations
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' @family Read Functions
#'
#' @inheritParams general
#' @inheritParams saveData
#'
#' @return `data.frame`.
#' @export
#'
#' @seealso
#' - [Ensembl](http://www.ensembl.org/info/website/upload/gff.html)
#' - [Gencode](http://www.gencodegenes.org/gencodeformat.html)
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
            col.names = gffCols,
            comment.char = "#",
            header = FALSE,
            stringsAsFactors = FALSE
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
