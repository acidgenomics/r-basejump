#' @inherit methods::show
#' @importFrom methods show
#' @name show
#' @export
#'
#' @examples
#' data(rse_small)
#'
#' ## Gene2Symbol ====
#' x <- Gene2Symbol(rse_small)
#' show(x)
#'
#' ## PANTHER ====
#' x <- PANTHER("Homo sapiens", progress = FALSE, .test = TRUE)
#' show(x)
NULL



.showHeader <- function(object) {
    cat(c(
        bold(paste(
            class(object),
            metadata(object)[["version"]]
        )),
        italic("https://steinbaugh.com/basejump"),
        "citation(\"basejump\")"
    ), sep = "\n")
}



.show.DataFrame <-  # nolint
    function(object) {
        .showHeader(object)
        show(as(object, "DataFrame"))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("EggNOG"),
    definition = function(object) {
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
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Ensembl2Entrez"),
    definition = .show.DataFrame
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Gene2Symbol"),
    definition = function(object) {
        .show.DataFrame(object)
        cat(paste0(
            length(unique(object[["geneID"]])), " genes; ",
            length(unique(object[["geneName"]])), " symbols"
        ), sep = "\n")
    }
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("HGNC2Ensembl"),
    definition = .show.DataFrame
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("MGI2Ensembl"),
    definition = .show.DataFrame
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = function(object) {
        .showHeader(object)
        showSlotInfo(list(
            organism = metadata(object)[["organism"]],
            release = metadata(object)[["release"]],
            genes = object[["geneID"]]
        ))
    }
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("Tx2Gene"),
    definition = function(object) {
        .show.DataFrame(object)
        cat(paste0(
            length(unique(object[["transcriptID"]])), " transcripts; ",
            length(unique(object[["geneID"]])), " genes"
        ), sep = "\n")
    }
)
