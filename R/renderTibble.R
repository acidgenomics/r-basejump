#' Render a tibble
#' @export
#' @importFrom knitr kable opts_knit
#' @importFrom tibble as_tibble
#' @keywords report
#' @param tibble \code{tibble}
#' @param caption Caption, to be used during a \code{knitr} call
#' @return \code{tibble} or \code{kable} depending on the calling environment
renderTibble <- function(tibble,
                         caption = NULL) {
    if (is.null(caption)) {
        stop("A caption is required.")
    }
    tibble <- tibble::as_tibble(tibble) %>%
        setNamesCamel
    if (!is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) & nrow(tibble) > 0) {
        knitr::kable(tibble,
                     booktabs = TRUE,
                     caption = caption,
                     longtable = TRUE,
                     row.names = FALSE)
    } else {
        return(tibble)
    }
}
