#' Ensembl Gene to Symbol Mappings
#'
#' @name gene2symbol
#' @family Gene Functions
#'
#' @inherit ensembl
#' @inheritParams general
#'
#' @return `data.frame`.
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
        release = NULL
    ) {
        ensembl(
            organism = object,
            format = "gene2symbol",
            genomeBuild = genomeBuild,
            release = release,
            return = "data.frame"
        )
    }
)
