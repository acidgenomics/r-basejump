#' Object summaries
#'
#' @name summary
#' @inherit base::summary description references
#' @importFrom S4Vectors summary
#'
#' @inheritParams params
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' data(
#'     RangedSummarizedExperiment,
#'     SummarizedExperiment_transcripts,
#'     package = "acidtest"
#' )
#' rse <- RangedSummarizedExperiment
#' txse <- SummarizedExperiment_transcripts
#'
#' ## Gene2Symbol ====
#' x <- Gene2Symbol(rse)
#' summary(x)
#'
#' ## Tx2Gene ====
#' x <- Tx2Gene(txse)
#' summary(x)
NULL



## Updated 2019-07-22.
`summary,Gene2Symbol` <-  # nolint
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
    definition = `summary,Gene2Symbol`
)



## Updated 2019-07-22.
`summary,Tx2Gene` <-  # nolint
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
    definition = `summary,Tx2Gene`
)
