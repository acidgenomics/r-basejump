#' Ensembl Gene to Symbol Mappings
#'
#' @rdname gene2symbol
#' @name gene2symbol
#' @family Gene Annotation Utilities
#'
#' @inheritParams general
#'
#' @return [data.frame].
#'
#' @examples
#' gene2symbol("Homo sapiens") %>% glimpse()
NULL



# Constructors =================================================================
.gene2symbol <- function(
    object,
    genomeBuild = NULL,
    release = NULL,
    uniqueSymbol = FALSE) {
    # Passthrough: genomeBuild, release, uniqueSymbol
    assert_is_a_string(object)
    annotable(
        object,
        format = "gene2symbol",
        genomeBuild = genomeBuild,
        release = release,
        uniqueSymbol = uniqueSymbol)
}



# Methods ======================================================================
#' @rdname gene2symbol
#' @export
setMethod(
    "gene2symbol",
    signature("character"),
    .gene2symbol)
