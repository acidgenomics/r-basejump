#' @name summary
#' @inherit base::summary title description references
#' @importFrom S4Vectors summary
#'
#' @inheritParams params
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' data(rse, tx_se)
#'
#' ## Gene2Symbol ====
#' x <- Gene2Symbol(rse)
#' summary(x)
#'
#' ## Tx2Gene ====
#' x <- Tx2Gene(tx_se)
#' summary(x)
NULL



summary.Gene2Symbol <-  # nolint
    function(object) {
        m <- metadata(object)
        showSlotInfo(list(
            genes = length(unique(object[["geneID"]])),
            symbols = length(unique(object[["geneName"]])),
            format = m[["format"]],
            organism = m[["organism"]],
            genomeBuild = m[["genomeBuild"]],
            ensemblRelease = m[["ensemblRelease"]],
            id = m[["id"]],
            version = as.character(m[["version"]]),
            date = m[["date"]]
        ))
    }



#' @rdname summary
#' @export
setMethod(
    f = "summary",
    signature = signature("Gene2Symbol"),
    definition = summary.Gene2Symbol
)



summary.Tx2Gene <-  # nolint
    function(object) {
        m <- metadata(object)
        showSlotInfo(list(
            transcripts = length(unique(object[["transcriptID"]])),
            genes = length(unique(object[["geneID"]])),
            organism = m[["organism"]],
            genomeBuild = m[["genomeBuild"]],
            ensemblRelease = m[["ensemblRelease"]],
            id = m[["id"]],
            version = as.character(m[["version"]]),
            date = m[["date"]]
        ))
    }



#' @rdname summary
#' @export
setMethod(
    f = "summary",
    signature = signature("Tx2Gene"),
    definition = summary.Tx2Gene
)
