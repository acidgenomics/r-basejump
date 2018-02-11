#' Transcript to Gene Mappings
#'
#' @rdname tx2gene
#' @name tx2gene
#' @family Gene Annotation Utilities
#'
#' @inheritParams AllGenerics
#' @inheritParams annotable
#'
#' @return [data.frame].
#'
#' @examples
#' tx2gene("Homo sapiens") %>% glimpse()
NULL



# Constructors =================================================================
.tx2gene <- function(
    object,
    genomeBuild = NULL,
    release = NULL,
    quiet = FALSE) {
    # Passthrough: genomeBuild, release, quiet
    assert_is_a_string(object)
    annotable(
        object,
        format = "tx2gene",
        genomeBuild = genomeBuild,
        release = release,
        quiet = quiet)
}



# Methods ======================================================================
#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("character"),
    .tx2gene)
