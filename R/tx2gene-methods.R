#' Transcript-to-Gene Mappings
#'
#' @name tx2gene
#' @family Data Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `tx2gene`.
#'
#' @examples
#' # SummarizedExperiment ====
#' x <- tx2gene(tx_se_small)
#' print(x)
NULL



.tx2gene.SE <-  # nolint
    function(object) {
        validObject(object)
        data <- rowData(object)
        rownames(data) <- rownames(object)
        if (!all(c("transcriptID", "geneID") %in% colnames(data))) {
            stop("Object does not contain transcript-to-gene mappings.")
        }
        .makeTx2gene(data)
    }



#' @rdname tx2gene
#' @export
setMethod(
    f = "tx2gene",
    signature = signature("SummarizedExperiment"),
    definition = .tx2gene.SE
)
