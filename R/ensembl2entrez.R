#' `ensembl2entrez` Generator
#'
#' 1:1 mappings from Ensembl gene IDs to Entrez IDs. Uses the oldest Entrez ID
#' if there are multiple identifiers that map to an Ensembl gene ID.
#'
#' @name ensembl2entrez
#' @family Annotation Functions
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `ensembl2entrez`.
NULL



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
        new("ensembl2entrez", data)
    }



#' @rdname ensembl2entrez
#' @export
setMethod(
    f = "ensembl2entrez",
    signature = signature("SummarizedExperiment"),
    definition = .ensembl2entrez.SE
)
