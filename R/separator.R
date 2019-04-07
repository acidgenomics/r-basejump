#' Separator bar
#'
#' Maximum of 72 characters wide.
#'
#' @note Bioconductor HTML vignettes don't render correctly when printing > 76
#'   characters, even though the default width is set at 80.
#'
#' @export
#'
#' @param sep `character(1)`.
#'   Separator character.
#' @param times `integer(1)`.
#'   Number of times to repeat.
#'
#' @return `character(1)`.
#'
#' @examples
#' separator(sep = "=", times = 10L)
separator <- function(
    sep = "=",
    times = min(c(
        getOption("width", default = 72L),
        72L
    ))
) {
    assert(
        isString(sep), nchar(sep) == 1L,
        isInt(times)
    )
    paste0(rep(x = sep, times = times), collapse = "")
}



# Soft deprecated, since this is still in use by bcbio R packages.
#' @rdname deprecated
#' @export
separatorBar <- separator()
