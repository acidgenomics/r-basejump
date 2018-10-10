#' @inherit Ensembl2Entrez-class
#'
#' @name ensembl2entrez
#' @family Identifier Mapping Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `Ensembl2Entrez`.
#'
#' @examples
#' data(rse_small)
#' x <- ensembl2entrez(rse_small)
#' print(x)
NULL



#' @rdname ensembl2entrez
#' @usage NULL
#' @export
Ensembl2Entrez <- function(object, ...) {
    ensembl2entrez(object, ...)
}



.ensembl2entrez.SE <-  # nolint
    function(object) {
        rowData <- rowData(object)
        assert_is_subset(
            x = c("entrezID", "geneID"),
            y = colnames(rowData)
        )
        entrezID <- rowData[["entrezID"]]
        assert_is_list(entrezID)
        names(entrezID) <- rowData[["geneID"]]
        # For genes that don't map 1:1 with Entrez, use the oldest Entrez ID.
        map <- lapply(entrezID, function(x) {
            if (all(is.na(x))) {
                NULL
            } else {
                sort(x)[[1L]]
            }
        })
        # Drop Ensembl genes that don't map to Entrez.
        map <- Filter(Negate(is.null), map)
        # Ensembl gene IDs are names. Entrez gene ID are values.
        vec <- unlist(map)
        data <- DataFrame(
            geneID = names(vec),
            entrezID = vec,
            row.names = names(vec)
        )
        new(Class = "Ensembl2Entrez", data)
    }



#' @rdname ensembl2entrez
#' @export
setMethod(
    f = "ensembl2entrez",
    signature = signature("SummarizedExperiment"),
    definition = .ensembl2entrez.SE
)
