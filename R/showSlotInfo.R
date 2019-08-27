#' Show slot information
#'
#' @note Updated 2019-08-27.
#' @export
#'
#' @param list `list`.
#'   Named list containing slot metadata.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' showSlotInfo(list(
#'     dir = "~",
#'     skip = NULL,
#'     filtered = TRUE
#' ))
showSlotInfo <- function(list) {
    assert(is.list(list))
    ## Consider checking for all atomic here.
    list <- Filter(f = Negate(is.null), x = list)
    list <- Filter(f = hasLength, x = list)
    ## Standardize to Bioconductor `show()` conventions.
    ## Refer to SummarizedExperiment method for example.
    out <- mapply(
        name = names(list),
        x = list,
        FUN = function(name, x) {
            if (length(x) == 1L) {
                paste0(name, ": ", x)
            } else {
                if (hasNames(x) && length(x) <= 4L) {
                    prefix <- paste0("[", names(x), "]")
                    info <- paste(prefix, x, sep = " ", collapse = "; ")
                } else {
                    info <- capture.output(headtail(x))
                }
                paste0(name, "(", length(x), "): ", info)
            }
        },
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )
    cat(unlist(out), sep = "\n")
}
