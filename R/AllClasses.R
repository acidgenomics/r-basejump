# EggNOG =======================================================================
#' EggNOG Database Annotations
#'
#' [EggNOG](http://eggnogdb.embl.de) is a database of biological information
#' hosted by the EMBL. It is based on the original idea of COGs (**c**lusters of
#' **o**rthologous **g**roups) and expands that idea to non-supervised
#' orthologous groups constructed from numerous organisms. eggNOG stands for
#' **e**volutionary **g**enealogy of **g**enes: **N**on-supervised
#' **O**rthologous **G**roups.
#'
#' @author Michael Steinbaugh
#' @inherit EggNOG
#' @export
#'
#' @return `EggNOG`. This class extends `list` and contains:
#'
#' 1. "`cogFunctionalCategories`": **C**luster of **O**rthologous **G**roups
#'    (COG) functional category information.
#' 2. "`annotations`": up-to-date functional descriptions and categories
#'    for **Eu**karyotes **N**on-supervised **O**rthologous **G**roups (euNOG)
#'    and **N**on-supervised **O**rthologous **G**roups (NOG) protein
#'    identifiers.
#'
#' The [EggNOG README file](http://eggnogdb.embl.de/download/latest/README.txt)
#' contains additional useful reference information.
setClass(Class = "EggNOG", contains = "SimpleDataFrameList")
setValidity(
    Class = "EggNOG",
    method = function(object) {
        validate_that(
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



# Ensembl2Entrez ===============================================================
#' Ensembl-to-Entrez Gene Identifier Mappings
#'
#' Defines 1:1 mappings from Ensembl gene IDs to Entrez IDs. Uses the oldest
#' Entrez ID if there are multiple identifiers that map to an Ensembl gene ID.
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @return `Ensembl2Entrez`. Contains a `DataFrame` with `geneID` and `entrezID`
#'   columns.
setClass(Class = "Ensembl2Entrez", contains = "DataFrame")
setValidity(
    Class = "Ensembl2Entrez",
    method = function(object) {
        validate_that(
            identical(
                x = colnames(object),
                y = c("geneID", "entrezID")
            ),
            !any(duplicated(object[["geneID"]])),
            class(object[["entrezID"]]) %in% c("integer", "list")
        )
    }
)



# Gene2Symbol ==================================================================
#' Gene-to-Symbol Mappings
#'
#' @note For some organisms, gene names and gene symbols do not map 1:1
#' (e.g. *Homo sapiens* and *Mus musculus*). Refer to the `format` argument here
#' in the documentation for approaches that deal with this issue.
#'
#' @section Genome metadata:
#'
#' We recommend slotting `organism`, `genomeBuild`, and `ensemblRelease` into
#' `S4Vectors::metadata()`.
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso `makeGene2Symbol`.
#'
#' @return `Gene2Symbol`. Contains a `DataFrame` with `geneID` and `geneName`
#'   columns.
setClass(Class = "Gene2Symbol", contains = "DataFrame")
setValidity(
    Class = "Gene2Symbol",
    method = function(object) {
        validate_that(
            identical(
                x = colnames(object),
                y = c("geneID", "geneName")
            ),
            nrow(object) > 0L,
            is.character(object[["geneID"]]),
            class(object[["geneName"]]) %in% c("character", "factor")
        )
    }
)



# HGNC2Ensembl =================================================================
#' HGNC-to-Ensembl Gene Identifier Mappings
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @return `HGNC2Ensembl`. Contains a `DataFrame` with `hgncID` and `geneID`
#'   columns.
setClass(Class = "HGNC2Ensembl", contains = "DataFrame")
setValidity(
    Class = "HGNC2Ensembl",
    method = function(object) {
        validate_that(
            identical(
                x = lapply(object, class),
                y = list(
                    hgncID = "integer",
                    geneID = "character"
                )
            ),
            identical(
                x = rownames(object),
                y = NULL
            )
        )
    }
)



# MGI2Ensembl ==================================================================
#' MGI-to-Ensembl Gene Identifier Mappings
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @return `MGI2Ensembl`. Contains a `DataFrame` with `mgiID` and `geneID`
#'   columns.
setClass(Class = "MGI2Ensembl", contains = "DataFrame")
setValidity(
    Class = "MGI2Ensembl",
    method = function(object) {
        validate_that(
            identical(
                x = colnames(object),
                y = c("mgiID", "geneID")
            ),
            is.null(rownames(object))
        )
    }
)



# PANTHER ======================================================================
#' PANTHER Database Annotations
#'
#' [PANTHER](http://www.pantherdb.org) gene ontology definitions. PANTHER stands
#' for **P**rotein **AN**alysis **TH**rough **E**volutionary **R**elationships.
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @return `PANTHER`. Contains a `DataFrame`.
setClass(Class = "PANTHER", contains = "DataFrame")
setValidity(
    Class = "PANTHER",
    method = function(object) {
        validate_that(
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



# Tx2Gene ======================================================================
#' Transcript-to-Gene Identifier Mappings
#'
#' @section Genome metadata:
#'
#' We recommend slotting `organism`, `genomeBuild`, and `ensemblRelease` into
#' `metadata()`.
#'
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso `makeTx2Gene`.
#'
#' @return `Tx2Gene`. Contains a `DataFrame` with `transcriptID` and `geneID`
#'   columns.
setClass(Class = "Tx2Gene", contains = "DataFrame")
setValidity(
    Class = "Tx2Gene",
    method = function(object) {
        validate_that(
            nrow(object) > 0L,
            identical(
                x = colnames(object),
                y = c("transcriptID", "geneID")
            ),
            all(vapply(
                X = object,
                FUN = is.character,
                FUN.VALUE = logical(1L)
            )),
            !any(duplicated(object[["transcriptID"]]))
        )
    }
)
