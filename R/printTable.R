#' Smart data.frame printing
#'
#' Display as `data.frame` in RMarkdown chunk output or `kable()` during a
#' `knitr` call
#' @export
#' @importFrom knitr kable opts_knit
#' @keywords report
#' @param df \code{data.frame}
#' @param ... Passthrough to \code{kable()} during \code{RMarkdown} rendering
#' with \code{knitr}
#' @return \code{data.frame} or \code{kable}, depending on the call
printTable <- function(df, ...) {
    # if (is.null(caption)) {
    #     stop("A caption is required.")
    # }
    df <- as.data.frame(df)
    if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) & nrow(df) > 0) {
        knitr::kable(df,
                     booktabs = TRUE,
                     longtable = TRUE,
                     ...)
    } else {
        return(df)
    }
}
