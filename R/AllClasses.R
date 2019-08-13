#' All classes
#' @include AllGlobals.R
#' @noRd
NULL



#' EggNOG database annotations
#'
#' [EggNOG](http://eggnogdb.embl.de) is a database of biological information
#' hosted by the EMBL. It is based on the original idea of COGs (**c**lusters of
#' **o**rthologous **g**roups) and expands that idea to non-supervised
#' orthologous groups constructed from numerous organisms. eggNOG stands for
#' **e**volutionary **g**enealogy of **g**enes: **N**on-supervised
#' **O**rthologous **G**roups.
#'
#' This class extends `list` and contains:
#'
#' 1. `"cogFunctionalCategories"`: **C**luster of **O**rthologous **G**roups
#'    (COG) functional category information.
#' 2. `"annotations"`: up-to-date functional descriptions and categories
#'    for **Eu**karyotes **N**on-supervised **O**rthologous **G**roups (euNOG)
#'    and **N**on-supervised **O**rthologous **G**roups (NOG) protein
#'    identifiers.
#'
#' The [EggNOG README](http://eggnogdb.embl.de/download/latest/README.txt)
#' contains additional useful reference information.
#'
#' @note Updated 2019-08-13.
#' @export
#'
#' @return `EggNOG`.
setClass(
    Class = "EggNOG",
    contains = "SimpleDataFrameList"
)
setValidity(
    Class = "EggNOG",
    method = function(object) {
        validate(
            identical(
                x = names(object),
                y = c("cogFunctionalCategories", "annotations")
            ),
            identical(
                x = colnames(object[["cogFunctionalCategories"]]),
                y = c("letter", "description")
            ),
            identical(
                x = colnames(object[["annotations"]]),
                y = c(
                    "eggnogID",
                    "consensusFunctionalDescription",
                    "cogFunctionalCategory"
                )
            )
        )
    }
)



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



#' PANTHER database annotations
#'
#' [PANTHER](http://www.pantherdb.org) gene ontology definitions. PANTHER stands
#' for **P**rotein **AN**alysis **TH**rough **E**volutionary **R**elationships.
#'
#' @note Updated 2019-08-08.
#' @export
#'
#' @return `PANTHER`.
setClass(
    Class = "PANTHER",
    contains = "DataFrame"
)
setValidity(
    Class = "PANTHER",
    method = function(object) {
        validate(
            identical(
                x = colnames(object),
                y = c(
                    "geneID",
                    "goBP",
                    "goCC",
                    "goMF",
                    "pantherClass",
                    "pantherFamilyName",
                    "pantherPathway",
                    "pantherSubfamilyID",
                    "pantherSubfamilyName"
                )
            ),
            all(c("organism", "release") %in% names(metadata(object)))
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
