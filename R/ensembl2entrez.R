#' @inherit Ensembl2Entrez-class
#'
#' @name Ensembl2Entrez
#' @family Identifier Mapping Functions
#' @export
#'
#' @inheritParams general
#'
#' @return `Ensembl2Entrez`.
#'
#' @examples
#' data(rse_small)
#' x <- Ensembl2Entrez(rse_small)
#' print(x)
NULL



.Ensembl2Entrez.SE <-  # nolint
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



#' @rdname Ensembl2Entrez
#' @export
setMethod(
    f = "Ensembl2Entrez",
    signature = signature("SummarizedExperiment"),
    definition = .Ensembl2Entrez.SE
)
