#' Transcripts per Million
#'
#' @name tpm
#' @inheritParams params
#'
#' @note Both gene- and transcript-level counts are supported, as long as they
#'   were imported using a tximport caller (e.g. salmon, kallisto).
#'
#' @examples
#' se <- SummarizedExperiment::SummarizedExperiment(
#'     assays = list(
#'         tpm = matrix(
#'             data = seq_len(4L),
#'             nrow = 2L,
#'             ncol = 2L,
#'             byrow = TRUE
#'         )
#'     )
#' )
#' x <- tpm(se)
#' class(x)
NULL



#' @importFrom SingleCellExperiment tpm
#' @aliases NULL
#' @export
SingleCellExperiment::tpm



#' @rdname tpm
#' @export
setMethod(
    f = "tpm",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        validObject(object)
        assertSubset("tpm", assayNames(object))
        assays(object)[["tpm"]]
    }
)
