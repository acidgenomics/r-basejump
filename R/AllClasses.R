# S3 classes ===================================================================
# tibble
setOldClass(Classes = class(tibble::tibble()))



# ensembl2entrez ===============================================================
#' ensembl2entrez Class
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
#' @inheritParams general
#'
#' @return `ensembl2entrez`.
#'
#' @seealso [ensembl2entrez()].
#'
#' @examples
#' x <- new(
#'     Class = "ensembl2entrez",
#'     DataFrame(
#'         geneID = "ENSG00000000003",
#'         entrezID = 7105L,
#'         row.names = "ENSG00000000003"
#'     )
#' )
#' print(x)
setClass(
    Class = "ensembl2entrez",
    contains = "DataFrame"
)

setValidity(
    Class = "ensembl2entrez",
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



# gene2symbol ==================================================================
#' gene2symbol Class
#'
#' Gene-to-symbol mappings.
#'
#' Contains a `DataFrame` with `geneID` and `geneName` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
#'
#' @return `gene2symbol`.
#'
#' @seealso
#' - [makeGene2symbol].
#' - [gene2symbol()].
#'
#' @examples
#' x <- new(
#'     Class = "gene2symbol",
#'     DataFrame(
#'         geneID = "ENSG00000000003",
#'         geneName = "TSPAN6",
#'         row.names = "ENSG00000000003"
#'     )
#' )
#' print(x)
setClass(
    Class = "gene2symbol",
    contains = "DataFrame"
)

setValidity(
    Class = "gene2symbol",
    method = function(object) {
        # Deprecate and move this assert check code in a future release.
        assertIsGene2symbol(object)
        TRUE
    }
)



# hgnc2ensembl =================================================================
#' hgnc2ensembl Class
#'
#' HGNC to Ensembl gene ID mappings.
#'
#' Contains a `DataFrame` with `hgncID` and `geneID` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @return `hgnc2ensembl`.
#'
#' @seealso [hgnc2ensembl()].
#'
#' @examples
#' x <- new(
#'     Class = "hgnc2ensembl",
#'     DataFrame(
#'         hgncID = 5L,
#'         geneID = "ENSG00000121410",
#'         row.names = 5L
#'     )
#' )
#' print(x)
setClass(
    Class = "hgnc2ensembl",
    contains = "DataFrame"
)

setValidity(
    Class = "hgnc2ensembl",
    method = function(object) {
        assert_are_identical(
            x = colnames(object),
            y = c("hgncID", "geneID")
        )
        assert_are_identical(
            x = rownames(object),
            y = as.character(object[["hgncID"]])
        )
        TRUE
    }
)



# mgi2ensembl ==================================================================
#' mgi2ensembl Class
#'
#' MGI to Ensembl gene ID mappings.
#'
#' Contains a `DataFrame` with `mgiID` and `geneID` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @return `mgi2ensembl`.
#'
#' @seealso [mgi2ensembl()].
#'
#' @examples
#' x <- new(
#'     Class = "mgi2ensembl",
#'     DataFrame(
#'         mgiID = 87853L,
#'         geneID = "ENSMUSG00000027596",
#'         row.names = 87853L
#'     )
#' )
#' print(x)
setClass(
    Class = "mgi2ensembl",
    contains = "DataFrame"
)

setValidity(
    Class = "mgi2ensembl",
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
#' PANTHER Class
#'
#' PANTHER Gene Ontology definitions.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @return `PANTHER`.
#'
#' @seealso [panther()].
setClass(
    Class = "PANTHER",
    contains = "DataFrame"
)

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



# tx2gene ======================================================================
#' tx2gene Class
#'
#' Transcript-to-gene mappings.
#'
#' Contains a `DataFrame` with `transcriptID` and `geneID` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
#'
#' @return `tx2gene`.
#'
#' @seealso
#' - [makeTx2gene].
#' - [tx2gene()].
#'
#' @examples
#' x <- new(
#'     "tx2gene",
#'     DataFrame(
#'         transcriptID = "ENST00000000233",
#'         geneID = "ENSG00000004059",
#'         row.names = "ENST00000000233"
#'     )
#' )
#' print(x)
setClass(
    Class = "tx2gene",
    contains = "DataFrame"
)

setValidity(
    Class = "tx2gene",
    method = function(object) {
        # Deprecate and move this assert check code in a future release.
        assertIsTx2gene(object)
        TRUE
    }
)
