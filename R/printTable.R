#' Smart data.frame print
#'
#' Display as `data.frame` in RMarkdown chunk or `kable()` during a knit
#'
#' @export
#' @importFrom knitr kable opts_knit
#' @importFrom tibble as_tibble rownames_to_column
#' @keywords report
#' @param df \code{data.frame}
#' @param ... Passthrough to \code{kable()} during \code{RMarkdown} rendering
#'   with \code{knitr}
#' @return \code{tibble} or \code{kable} for chunk output, depending on the call
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
