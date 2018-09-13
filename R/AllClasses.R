# tbl_df =======================================================================
setOldClass(Classes = class(tibble()))



# gene2symbol ==================================================================
#' `gene2symbol` Class
#'
#' Gene-to-symbol mappings.
#'
#' Contains a `DataFrame` with `geneID` and `geneName` columns.
#'
#' @family S4 Object
#' @author Michael Steinbaugh
#' @export
#'
#' @inheritParams general
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



# tx2gene ==================================================================
#' `tx2gene` Class
#'
#' Transcript-to-gene mappings.
#'
#' Contains a `DataFrame` with `transcriptID` and `geneID` columns.
#'
#' @export
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
