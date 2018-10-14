# TODO Consider limiting to console width per line.



#' Show Slot Information
#'
#' @export
#'
#' @param list `list`. Named list containing slot metadata.
showSlotInfo <- function(list) {
    assert_is_list(list)
    stopifnot(all(vapply(
        X = list,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    )))
    list <- Filter(f = Negate(is.null), x = list)
    list <- Filter(f = has_length, x = list)
    # Standardize to Bioconductor `show()` conventions.
    # Refer to SummarizedExperiment method for example.
    out <- mapply(
        name = names(list),
        x = list,
        FUN = function(name, x) {
            if (has_length(x, n = 1L)) {
                paste0(name, ": ", x)
            } else {
                if (has_names(x) && length(x) <= 4L) {
                    prefix <- paste0("[", names(x), "]")
                    info <- paste(prefix, x, sep = " ", collapse = "; ")
                } else {
                    info <- capture.output(headtail(x))
                }
                paste0(name, "(", length(x), "): ", info)
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    cat(out, sep = "\n")
}
