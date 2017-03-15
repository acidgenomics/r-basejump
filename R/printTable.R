#' Smart data.frame print
#'
#' Display as \code{data.frame} in RMarkdown chunk or \code{kable()} during knit
#'
#' @author Michael Steinbaugh
#' @keywords report
#'
#' @import knitr
#' @import tibble
#'
#' @param df \code{data.frame}
#' @param rownames Print rownames (\code{TRUE/FALSE})
#' @param ... Passthrough to \code{kable()} during \code{RMarkdown} rendering
#'   with \code{knitr}
#'
#' @return \code{tibble} or \code{kable} for chunk output, depending on the call
#'
#' @export
printTable <- function(df, rownames = FALSE, ...) {
    # if (is.null(caption)) {
    #     stop("A caption is required.")
    # }
    df <- as.data.frame(df)

    # Drop the rownames, if desired
    if (!isTRUE(rownames)) {
        rownames(df) <- NULL
    }

    if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) & nrow(df) > 0) {
        knitr::kable(df,
                     booktabs = TRUE,
                     longtable = TRUE,
                     ...)
    } else {
        return(df)
    }
}
