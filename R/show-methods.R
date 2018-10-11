# FIXME EggNOG
# FIXME PANTHER



#' @inherit methods::show
#' @importFrom methods show
#' @name show
#' @export
#'
#' @examples
#' ## PANTHER ====
#' x <- panther("Homo sapiens", .test = TRUE)
#' show(x)
NULL



# Internals ====================================================================
# TODO Add maxwidth handling in a future update.
.slotInfo <- function(
    object,
    name = NULL,
    unique = FALSE
) {
    if (is.null(name)) {
        name <- deparse(substitute(object))
    }
    if (isTRUE(unique)) {
        object <- unique(object)
    }
    length <- length(object)
    headtail <- capture.output(headtail(object))
    paste0(name, "(", length, "): ", headtail)
}



# Tx2Gene ======================================================================
# message(paste(
#     "Mappings:",
#     length(unique(data[["transcriptID"]])), "transcripts,",
#     length(unique(data[["geneID"]])), "genes"
# ))



# PANTHER ======================================================================
# FIXME Slot metadata into the data frame that we can show.
.show.PANTHER <-  # nolint
    function(object) {
        genes <- .slotInfo(object[["geneID"]], name = "genes")
        cat(genes)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = .show.PANTHER
)
