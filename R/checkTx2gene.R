#' Check Transcript to Gene Mapping Data
#'
#' @param object [data.frame] containing Ensembl transcript to gene identifier
#'   mappings. Must be structured as a two column [data.frame] with "enstxp" and
#'   "ensgene" columns.
#'
#' @return Silent on pass, stop on error.
#' @export
#'
#' @examples
#' # Success
#' tx2gene <- annotable("Homo sapiens", format = "tx2gene")
#' checkTx2gene(tx2gene)
#'
#' # Failure
#' \dontrun{
#' checkTx2gene(mtcars)
#' }
checkTx2gene <- function(object) {
    if (!is.data.frame(object)) {
        stop("tx2gene must be 'data.frame' class object", call. = FALSE)
    }
    colnames <- c("enstxp", "ensgene")
    if (!identical(colnames(object), colnames)) {
        stop(paste(
            "tx2gene must contain:", toString(colnames)
        ), call. = FALSE)
    }
}
