#' All classes
#' @include AllGlobals.R
#' @noRd
NULL



#' Ensembl-to-Entrez gene identifier mappings
#'
#' Defines 1:1 mappings from Ensembl gene IDs to Entrez IDs. Uses the oldest
#' Entrez ID if there are multiple identifiers that map to an Ensembl gene ID.
#'
#' Contains a `DataFrame` with `geneID` and `entrezID` columns.
#'
#' @note Updated 2019-08-08.
#' @export
#'
#' @return `Ensembl2Entrez`.
setClass(
    Class = "Ensembl2Entrez",
    contains = "DataFrame"
)
setValidity(
    Class = "Ensembl2Entrez",
    method = function(object) {
        validate(
            identical(colnames(object), c("geneID", "entrezID")),
            is.integer(object[["entrezID"]])
        )
    }
)



#' Gene-to-symbol mappings
#'
#' @details
#' Contains a `DataFrame` with `geneID` and `geneName` columns.
#'
#' @section Genome metadata:
#'
#' We recommend slotting `organism`, `genomeBuild`, and `ensemblRelease` into
#' [`metadata()`][S4Vectors::metadata].
#'
#' @note Updated 2019-08-08.
#' @export
#'
#' @return `Gene2Symbol`.
setClass(
    Class = "Gene2Symbol",
    contains = "DataFrame"
)
setValidity(
    Class = "Gene2Symbol",
    method = function(object) {
        validate(
            identical(colnames(object), c("geneID", "geneName")),
            nrow(object) > 0L,
            is.character(object[["geneID"]]),
            class(object[["geneName"]]) %in% c("character", "factor")
        )
    }
)



#' HGNC-to-Ensembl gene identifier mappings
#'
#' @details
#' Contains a `DataFrame` with `hgncID` and `geneID` columns.
#'
#' @note Updated 2019-08-08.
#' @export
#'
#' @return `HGNC2Ensembl`.
setClass(
    Class = "HGNC2Ensembl",
    contains = "DataFrame"
)
setValidity(
    Class = "HGNC2Ensembl",
    method = function(object) {
        validate(
            identical(
                x = lapply(object, class),
                y = list(
                    hgncID = "integer",
                    geneID = "character"
                )
            ),
            is.null(rownames(object))
        )
    }
)



#' MGI-to-Ensembl gene identifier mappings
#'
#' @details
#' Contains a `DataFrame` with `mgiID` and `geneID` columns.
#'
#' @note Updated 2019-08-08.
#' @export
#'
#' @return `MGI2Ensembl`.
setClass(
    Class = "MGI2Ensembl",
    contains = "DataFrame"
)
setValidity(
    Class = "MGI2Ensembl",
    method = function(object) {
        validate(
            identical(colnames(object), c("mgiID", "geneID")),
            is.null(rownames(object))
        )
    }
)



#' Transcript-to-gene identifier mappings
#'
#' @details
#' Contains a `DataFrame` with `transcriptID` and `geneID` columns.
#'
#' @section Genome metadata:
#'
#' We recommend slotting `organism`, `genomeBuild`, and `ensemblRelease` into
#' `metadata`.
#'
#' @note Updated 2019-08-08.
#' @export
#'
#' @return `Tx2Gene`.
setClass(
    Class = "Tx2Gene",
    contains = "DataFrame"
)
setValidity(
    Class = "Tx2Gene",
    method = function(object) {
        validate(
            nrow(object) > 0L,
            identical(colnames(object), c("transcriptID", "geneID")),
            all(vapply(
                X = object,
                FUN = is.character,
                FUN.VALUE = logical(1L)
            )),
            !any(duplicated(object[["transcriptID"]]))
        )
    }
)
