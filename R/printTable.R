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
#' @param caption Caption (optional)
#' @param rownames Print rownames (\code{TRUE/FALSE})
#' @param ... Passthrough to \code{kable()} during RMarkdown render with
#'   \code{knitr}
#'
#' @return Tibble or kable for chunk output, depending on the call
#'
#' @export
printTable <- function(df, caption = NULL, rownames = FALSE, ...) {
    if (is.null(caption)) {
        message("no table caption provided")
    }
    df <- as.data.frame(df)
    # Only proceed if there are rows
    if (nrow(df) > 0) {
        # Drop the rownames, if desired
        if (!isTRUE(rownames)) {
            rownames(df) <- NULL
        }
        # Check for knit call and use `kable()`
        if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))) {
            # `booktabs` and `longtable` improve PDF table handling
            knitr::kable(df,
                         booktabs = TRUE,
                         caption = caption,
                         longtable = TRUE,
                         ...)
        } else {
            return(df)
        }
    } else {
        print("0 rows")
    }
}
