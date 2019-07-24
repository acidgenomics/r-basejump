#' Transcripts per million
#'
#' @note Both gene- and transcript-level counts are supported, as long as they
#'   were imported using a tximport caller (e.g. salmon, kallisto).
#'
#' @name tpm
#' @inheritParams params
#'
#' @return `matrix`.
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



#' @rdname tpm
#' @name tpm
#' @importFrom SingleCellExperiment tpm
#' @export
NULL



## Updated 2019-07-22.
`tpm,SummarizedExperiment` <-  # nolint
    function(object) {
        validObject(object)
        assert(isSubset("tpm", assayNames(object)))
        assays(object)[["tpm"]]
    }



#' @rdname tpm
#' @export
setMethod(
    f = "tpm",
    signature = signature("SummarizedExperiment"),
    definition = `tpm,SummarizedExperiment`
)
