#' @importFrom methods show
#' @aliases NULL
#' @export
methods::show



#' @inherit methods::show
#' @name show
#'
#' @examples
#' data(rse_small)
#' options(basejump.test = TRUE)
#'
#' ## Gene2Symbol ====
#' x <- Gene2Symbol(rse_small)
#' show(x)
#'
#' ## PANTHER ====
#' x <- PANTHER("Homo sapiens", progress = FALSE)
#' show(x)
NULL



# Internal =====================================================================
.showHeader <- function(object) {
    cat(paste(class(object), metadata(object)[["version"]]), sep = "\n")
}



show.DataFrame <-  # nolint
    function(object) {
        .showHeader(object)
        data <- as(object, "DataFrame")
        show(data)
    }



# EggNOG =======================================================================
show.EggNOG <-  # nolint
    function(object) {
        .showHeader(object)
        showSlotInfo(list(
            ids = object %>%
                .[["annotations"]] %>%
                .[["eggnogID"]] %>%
                sort(),
            categories = object %>%
                .[["cogFunctionalCategories"]] %>%
                .[["description"]] %>%
                sort()
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("EggNOG"),
    definition = show.EggNOG
)



# Ensembl2Entrez ===============================================================
#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Ensembl2Entrez"),
    definition = show.DataFrame
)



# Gene2Symbol ==================================================================
show.Gene2Symbol <-  # nolint
    function(object) {
        show.DataFrame(object)
        cat(paste0(
            length(unique(object[["geneID"]])), " genes; ",
            length(unique(object[["geneName"]])), " symbols"
        ), sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Gene2Symbol"),
    definition = show.Gene2Symbol
)



# HGNC2Ensembl =================================================================
#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("HGNC2Ensembl"),
    definition = show.DataFrame
)



# MGI2Ensembl ==================================================================
#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("MGI2Ensembl"),
    definition = show.DataFrame
)



# PANTHER ======================================================================
show.PANTHER <-  # nolint
    function(object) {
        .showHeader(object)
        showSlotInfo(list(
            organism = metadata(object)[["organism"]],
            release = metadata(object)[["release"]],
            genes = object[["geneID"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = show.PANTHER
)



# Tx2Gene ======================================================================
show.Tx2Gene <-  # nolint
    function(object) {
        show.DataFrame(object)
        cat(paste0(
            length(unique(object[["transcriptID"]])), " transcripts; ",
            length(unique(object[["geneID"]])), " genes"
        ), sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Tx2Gene"),
    definition = show.Tx2Gene
)
