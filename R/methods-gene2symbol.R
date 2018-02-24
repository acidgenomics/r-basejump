#' Ensembl Gene to Symbol Mappings
#'
#' @rdname gene2symbol
#' @name gene2symbol
#' @family Gene Annotation Utilities
#'
#' @inherit ensembl
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
        assert_is_a_string(object)
        assert_is_a_bool(uniqueSymbol)
        data <- ensembl(
            organism = object,
            format = "gene2symbol",
            genomeBuild = genomeBuild,
            release = release,
            return = "data.frame")
        if (isTRUE(uniqueSymbol)) {
            data <- .uniqueSymbol(data)
        }
        data
    })
