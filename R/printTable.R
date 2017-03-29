#' Smart data.frame print
#'
#' Display as data frame in RMarkdown chunk or \code{kable()} during knit
#'
#' @author Michael Steinbaugh
#'
#' @import knitr
#' @import tibble
#'
#' @param df Data frame
#' @param rownames Print rownames (\code{TRUE/FALSE})
#' @param ... Passthrough to \code{kable()} during RMarkdown render with
#'   \code{knitr}
#'
#' @return Tibble or kable for chunk output, depending on the call
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
        print(df)
    }
}
