#' kable rework with custom defaults
#'
#' Display as data frame in RMarkdown chunk or \code{kable()} during knit
#'
#' @author Michael Steinbaugh
#' @keywords internal
#'
#' @import knitr
#'
#' @param x An R object (typically a matrix or data frame)
#' @param ... Pass through to \code{knitr::kable()}
#'
#' @export
#'
#' @examples
#' kable(head(iris))
kable <- function(x, ...) {
    # Improve appearance in RStudio
    if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
        # Customize defaults
        return(
            knitr::kable(x,
                         ...,
                         # booktabs = TRUE,
                         longtable = TRUE)
        )
    } else {
        return(x)
    }
}
