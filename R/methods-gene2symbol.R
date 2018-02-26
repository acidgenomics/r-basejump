#' Ensembl Gene to Symbol Mappings
#'
#' @rdname gene2symbol
#' @name gene2symbol
#' @family Gene Functions
#'
#' @inherit ensembl
#' @inheritParams general
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
        uniqueSymbol = FALSE) {
        ensembl(
            organism = object,
            format = "gene2symbol",
            genomeBuild = genomeBuild,
            release = release,
            uniqueSymbol = uniqueSymbol,
            sanitizeColnames = TRUE,
            return = "data.frame")
    })
