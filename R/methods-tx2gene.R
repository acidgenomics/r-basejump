#' Transcript to Gene Mappings
#'
#' @rdname tx2gene
#' @name tx2gene
#' @family Gene Annotation Utilities
#'
#' @inheritParams general
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
    release = NULL) {
    assert_is_a_string(object)
    annotable(
        object,
        format = "tx2gene",
        genomeBuild = genomeBuild,
        release = release)
}



# Methods ======================================================================
#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("character"),
    .tx2gene)
