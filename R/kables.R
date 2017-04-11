#' Handle multiple kables in a single RMarkdown chunk
#'
#' Utility function for improving knit reports
#'
#' @author Michael Steinbaugh
#'
#' @importFrom knitr asis_output opts_knit
#'
#' @param list List of tables (e.g. data frame, matrix)
#'
#' @return Knit tables, using \code{kable}
#' @export
#'
#' @examples
#' kables(list(example1 = iris, example2 = mtcars))
kables <- function(list) {
    output <- opts_knit$get("rmarkdown.pandoc.to")
    if (!is.null(output)) {
        # Need to fix caption handling for PDFs
        tables <- lapply(seq_along(list), function(a) {
            kable(list[a], caption = "XXX")
        })
        return(asis_output(tables))
    } else {
        return(list)
    }
}
