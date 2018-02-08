#' Ensembl Gene to Symbol Mappings
#'
#' @rdname gene2symbol
#' @name gene2symbol
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
#'
#' @return [data.frame].
#'
#' @examples
#' gene2symbol("Homo sapiens") %>% glimpse()
NULL



# Methods ======================================================================
#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("character"),
    function(
        object,
        genomeBuild = NULL,
        release = NULL,
        uniqueSymbol = FALSE,
        quiet = FALSE) {
        annotable(
            object,
            format = "gene2symbol",
            genomeBuild = genomeBuild,
            release = release,
            uniqueSymbol = uniqueSymbol,
            quiet = quiet)
    })
