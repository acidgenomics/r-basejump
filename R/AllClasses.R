# TODO Define GFF class.
# TODO Consider defining ensembl2entrez class.



# tbl_df =======================================================================
setOldClass(Classes = class(tibble()))



# gene2symbol ==================================================================
#' `gene2symbol` Class
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
#' @examples
#' x <- new(
#'     "gene2symbol",
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
        assertIsGene2symbol(object)
        TRUE
    }
)



# hgnc2ensembl =================================================================
#' `hgnc2ensembl` Class
#'
#' HGNC to Ensembl gene ID mappings.
#'
#' Contains a `DataFrame` with `hgncID` and `geneID` columns.
#'
#' @family S4 Classes
#' @author Michael Steinbaugh
#' @export
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



# mgi2gene =====================================================================
#' `mgi2ensembl` Class
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
#' @examples
#' x <- (
#'     "mgi2gene",
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



# tx2gene ======================================================================
#' `tx2gene` Class
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
        assertIsTx2gene(object)
        TRUE
    }
)
