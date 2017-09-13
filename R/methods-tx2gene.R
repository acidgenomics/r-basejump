#' Convert Ensembl Transcript to Gene
#'
#' @rdname tx2gene
#' @name tx2gene
#'
#' @inheritParams annotable
#'
#' @return Same class as object.
#' @export
#'
#' @examples
#' # character
#' c("ENSMUST00000000001",
#'   "ENSMUST00000000003",
#'   "ENSMUST00000114041") %>% tx2gene
#'
#' \dontrun{
#' # matrix
#' matrix(
#'     data = seq(1L:6L),
#'     byrow = TRUE,
#'     nrow = 3L,
#'     ncol = 2L,
#'     dimnames = list(c("ENSMUST00000000001",
#'                       "ENSMUST00000000003",
#'                       "ENSMUST00000114041"),
#'                     c("sample1",
#'                       "sample2"))) %>% tx2gene
#' }
NULL



# Constructors ====
.t2gvec <- function(object, release = "current") {
    # Prevent pass in of genomeBuild as primary object.
    # Improve this in a future update.
    if (is_string(object)) {
        stop("tx2gene conversion requires > 1 identifier")
    }
    if (any(is.na(object))) {
        stop("NA identifier detected", call. = FALSE)
    }
    if (any(object == "")) {
        stop("Empty string identifier detected", call. = FALSE)
    }
    organism <- detectOrganism(object[[1L]])
    t2g <- annotable(organism, format = "tx2gene", release = release) %>%
        .[object, ] %>%
        .[!is.na(.[["ensgene"]]), ]
    gene <- t2g[["ensgene"]]
    names(gene) <- t2g[["enstxp"]]
    if (!all(object %in% names(gene))) {
        stop(paste(
            "Unmatched transcripts present.",
            "Try using a GTF file instead."),
            call. = FALSE)
    }
    gene[object]
}



.t2gdim <- function(object, release = "current") {
    rownames(object) <- rownames(object) %>%
        .t2gvec(release = release)
    object
}



# Methods ====
#' @rdname tx2gene
setMethod("tx2gene", "character", .t2gvec)



#' @rdname tx2gene
#' @export
setMethod("tx2gene", "data.frame", .t2gdim)



#' @rdname tx2gene
#' @export
setMethod("tx2gene", "DataFrame", .t2gdim)



#' @rdname tx2gene
#' @export
setMethod("tx2gene", "dgCMatrix", .t2gdim)



#' @rdname tx2gene
#' @export
setMethod("tx2gene", "dgTMatrix", .t2gdim)



#' @rdname tx2gene
#' @export
setMethod("tx2gene", "matrix", .t2gdim)
