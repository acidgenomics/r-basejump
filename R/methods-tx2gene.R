#' Transcript to Gene Mappings
#'
#' @rdname tx2gene
#' @name tx2gene
#' @family Gene Annotation Utilities
#'
#' @inheritParams ensembl
#'
#' @return [data.frame].
#'
#' @examples
#' tx2gene("Homo sapiens") %>% glimpse()
NULL



# Methods ======================================================================
#' @rdname tx2gene
#' @export
setMethod(
    "tx2gene",
    signature("character"),
    function(
        object,
        genomeBuild = NULL,
        release = NULL) {
        assert_is_a_string(object)
        ensembl(
            object,
            format = "tx2gene",
            genomeBuild = genomeBuild,
            release = release,
            return = "data.frame")
    })
