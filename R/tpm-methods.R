#' @rdname tpm
#' @importFrom SingleCellExperiment tpm
#' @export
tpm <- SingleCellExperiment::tpm



#' Transcripts Per Million (TPM)
#'
#' @note Both gene- and transcript-level counts are supported, as long as they
#'   were imported using a tximport caller (e.g. salmon, kallisto).
#'
#' @name tpm
#' @family SummarizedExperiment Functions
#'
#' @inheritParams general
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
#' @export
setMethod(
    f = "tpm",
    signature = signature("SummarizedExperiment"),
    definition = function(object) {
        validObject(object)
        assert_is_subset("tpm", assayNames(object))
        assays(object)[["tpm"]]
    }
)
