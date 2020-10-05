#' All classes
#' @include AllGlobals.R
#' @noRd
NULL



## > showClass("missingOrNULL")
setClassUnion(name = "missingOrNULL", members = c("missing", "NULL"))



#' Ensembl-to-Entrez gene identifier mappings
#'
#' @details
#' Contains a `DataFrame` with `ensembl` and `entrez` columns.
#'
#' @export
#' @note Updated 2020-10-01.
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
            identical(
                x = c("ensembl", "entrez"),
                y = colnames(object)
            ),
            is.integer(object[["entrez"]])
        )
    }
)



#' Entrez-to-Ensembl gene identifier mappings
#'
#' @inherit Ensembl2Entrez-class details
#'
#' @export
#' @note Updated 2020-10-01.
#'
#' @return `Entrez2Ensembl`.
setClass(
    Class = "Entrez2Ensembl",
    contains = "DataFrame"
)
setValidity(
    Class = "Entrez2Ensembl",
    method = function(object) {
        validate(
            identical(
                x = c("entrez", "ensembl"),
                y = colnames(object)
            ),
            is.integer(object[["entrez"]])
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
#' @export
#' @note Updated 2019-08-08.
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
            isAny(object[["geneName"]], c("character", "factor"))
        )
    }
)



#' HGNC complete set metadata
#'
#' @export
#' @note Updated 2020-10-03
#'
#' @return `HGNC`.
setClass(
    Class = "HGNC",
    contains = "DataFrame"
)
setValidity(
    Class = "HGNC",
    method = function(object) {
        validate(
            isSubset(
                x = c("hgncID", "ensemblGeneID"),
                y = colnames(object)
            )
        )
    }
)



#' HGNC-to-Ensembl gene identifier mappings
#'
#' @details
#' Contains a `DataFrame` with `hgnc` and `ensembl` columns.
#'
#' @export
#' @note Updated 2019-08-08.
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
                x = c("hgnc", "ensembl"),
                y = colnames(object)
            )
        )
    }
)



#' MGI-to-Ensembl gene identifier mappings
#'
#' @details
#' Contains a `DataFrame` with `mgi` and `ensembl` columns.
#'
#' @export
#' @note Updated 2020-10-05.
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
            identical(colnames(object), c("mgi", "ensembl"))
        )
    }
)



#' Protein-to-gene mappings
#'
#' @details
#' Contains a `DataFrame` with `proteinID`, `geneID`, and `geneName` columns.
#'
#' @section Genome metadata:
#'
#' We recommend slotting `organism`, `genomeBuild`, and `ensemblRelease` into
#' [`metadata()`][S4Vectors::metadata].
#'
#' @export
#' @note Updated 2020-09-25.
#'
#' @return `Protein2Gene`.
setClass(
    Class = "Protein2Gene",
    contains = "DataFrame"
)
setValidity(
    Class = "Protein2Gene",
    method = function(object) {
        validate(
            identical(
                x = colnames(object),
                y = c("proteinID", "geneID", "geneName")
            ),
            nrow(object) > 0L,
            all(bapply(X = object, FUN = is.character))
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
#' @export
#' @note Updated 2019-08-08.
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
