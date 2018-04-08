gffCols <- c(
    "seqname",
    "source",
    "feature",
    "start",
    "end",
    "score",
    "strand",
    "frame",
    "attribute"
)



#' Read GFF Annotations
#'
#' The GFF (General Feature Format) format consists of one line per feature,
#' each containing 9 columns of data, plus optional track definition lines. The
#' GTF (General Transfer Format) is identical to GFF version 2.
#'
#' Column names follow the
#' [conventions](https://ensembl.org/info/website/upload/gff.html)
#' defined by Ensembl.
#'
#' @family Read Functions
#' @author Michael Steinbaugh
#'
#' @inheritParams general
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
readGFF <- function(file) {
    assert_is_a_string(file)
    assert_all_are_matching_regex(file, "\\.g(f|t)f(\\d)?(\\.gz)?$")
    file <- localOrRemoteFile(file)
    message(paste("Reading GFF:", names(file)))
    gff <- tryCatch(
        read.delim(
            file = file,
            col.names = gffCols,
            comment.char = "#",
            header = FALSE,
            stringsAsFactors = FALSE
        ),
        error = function(e) {
            stop("GFF file failed to load")
        },
        warning = function(w) {
            stop("GFF file failed to load")
        }
    )
    gff
}



# Aliases ======================================================================
#' @rdname readGFF
#' @usage NULL
#' @export
readGFF -> readGTF
