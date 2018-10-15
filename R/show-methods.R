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



.showHeader <- function(object, version = NULL) {
    cat(c(
        bold(paste(class(object), version)),
        italic("https://steinbaugh.com/basejump"),
        "citation(\"basejump\")"
    ), sep = "\n")
}



# Tx2Gene ======================================================================
# message(paste(
#     "Mappings:",
#     length(unique(data[["transcriptID"]])), "transcripts,",
#     length(unique(data[["geneID"]])), "genes"
# ))



# PANTHER ======================================================================
# FIXME Slot version into PANTHER metadata.
.show.PANTHER <-  # nolint
    function(object) {
        showSlotInfo(list(
            genes = object[["geneID"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = .show.PANTHER
)
