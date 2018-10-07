# S3 classes ===================================================================
# tibble
# Note that `tbl_df` is exported in tibble v1.4.99 (see GitHub).
#' @importFrom methods setOldClass
setOldClass(Classes = class(tibble::tibble()))



# EggNOG =======================================================================
#' `EggNOG` Class
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
#' 1. "`cogFunctionalCategories`": **C**luster of **O**rthologous **G**roups
#'    (COG) functional category information.
#' 2. "`annotations`": up-to-date functional descriptions and categories
#'    for **Eu**karyotes **N**on-supervised **O**rthologous **G**roups (euNOG)
#'    and **N**on-supervised **O**rthologous **G**roups (NOG) protein
#'    identifiers.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso
#' - [eggnog()].
#' - [EggNOG README](http://eggnogdb.embl.de/download/latest/README.txt).
setClass(Class = "EggNOG", contains = "list")

# FIXME Add validity.



# Ensembl2Entrez ===============================================================
#' `Ensembl2Entrez` Class
#'
#' Ensembl gene ID to Entrez ID mappings.
#'
#' Contains a `DataFrame` with `geneID` and `entrezID` columns. Requires 1:1
#' mappings.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [Ensembl2Entrez()].
setClass(Class = "Ensembl2Entrez", contains = "DataFrame")

setValidity(
    Class = "Ensembl2Entrez",
    method = function(object) {
        assert_are_identical(
            x = colnames(object),
            y = c("geneID", "entrezID")
        )
        assert_are_identical(
            x = rownames(object),
            y = as.character(object[["geneID"]])
        )
        assert_has_no_duplicates(object[["geneID"]])
        assert_is_integer(object[["entrezID"]])
        TRUE
    }
)



# Gene2Symbol ==================================================================
#' `Gene2Symbol` Class
#'
#' Gene-to-symbol mappings.
#'
#' Contains a `DataFrame` with `geneID` and `geneName` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [gene2symbol()].
setClass(Class = "Gene2Symbol", contains = "DataFrame")

setValidity(
    Class = "Gene2Symbol",
    method = function(object) {
        assert_are_identical(
            x = colnames(object),
            y = c("geneID", "geneName")
        )
        assert_has_rows(object)
        # Assert that all columns are character.
        invisible(lapply(object, assert_is_character))
        # Assert that neither column has duplicates.
        invisible(lapply(object, assert_has_no_duplicates))
        stopifnot(all(complete.cases(object)))
        TRUE
    }
)



# HGNC2Ensembl =================================================================
#' `HGNC2Ensembl` Class
#'
#' HGNC-to-Ensembl gene ID mappings.
#'
#' Contains a `DataFrame` with `hgncID` and `geneID` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [hgnc2ensembl()].
setClass(Class = "HGNC2Ensembl", contains = "DataFrame")

setValidity(
    Class = "HGNC2Ensembl",
    method = function(object) {
        assert_are_identical(
            x = lapply(object, class),
            y = list(
                hgncID = "integer",
                geneID = "character"
            )
        )
        assert_are_identical(
            x = rownames(object),
            y = NULL
        )
        TRUE
    }
)



# MGI2Ensembl ==================================================================
#' `MGI2Ensembl` Class
#'
#' MGI-to-Ensembl gene ID mappings.
#'
#' Contains a `DataFrame` with `mgiID` and `geneID` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [mgi2ensembl()].
setClass(Class = "MGI2Ensembl", contains = "DataFrame")

setValidity(
    Class = "MGI2Ensembl",
    method = function(object) {
        assert_are_identical(
            x = colnames(object),
            y = c("mgiID", "geneID")
        )
        assert_are_identical(
            x = rownames(object),
            y = as.character(object[["mgiID"]])
        )
        TRUE
    }
)



# PANTHER ======================================================================
#' `PANTHER` Class
#'
#' [PANTHER](http://www.pantherdb.org) Gene Ontology definitions. PANTHER stands
#' for **P**rotein **AN**alysis **TH**rough **E**volutionary **R**elationships.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso [panther()].
setClass(Class = "PANTHER", contains = "DataFrame")

setValidity(
    Class = "PANTHER",
    method = function(object) {
        assert_are_identical(
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
        )
        TRUE
    }
)



# Tx2Gene ======================================================================
#' `Tx2Gene` Class
#'
#' Transcript-to-gene mappings.
#'
#' Contains a `DataFrame` with `transcriptID` and `geneID` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @seealso
#' - [makeTx2Gene].
#' - [tx2gene()].
setClass(Class = "Tx2Gene", contains = "DataFrame")

setValidity(
    Class = "Tx2Gene",
    method = function(object) {
        assert_has_rows(object)
        assert_are_identical(
            x = colnames(object),
            y = c("transcriptID", "geneID")
        )
        # Assert that all columns are character.
        invisible(lapply(object, assert_is_character))
        # Assert that there are no duplicate transcripts.
        assert_has_no_duplicates(object[["transcriptID"]])
        stopifnot(all(complete.cases(object)))
        TRUE
    }
)
