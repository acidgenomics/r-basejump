#' Transcripts per million
#'
#' @note Both gene- and transcript-level counts are supported, as long as they
#'   were imported using a tximport caller (e.g. salmon, kallisto).
#' @note Updated 2019-08-06.
#'
#' @name tpm
#' @inheritParams AcidRoxygen::params
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



## Updated 2019-08-06.
`tpm,SummarizedExperiment` <-  # nolint
    function(object) {
        assay(object, i = "tpm")
    }



#' @rdname tpm
#' @export
setMethod(
    f = "tpm",
    signature = signature("SummarizedExperiment"),
    definition = `tpm,SummarizedExperiment`
)
