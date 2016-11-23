#' Render a tibble when necessary
#'
#' @import knitr
#'
#' @param tibble Input tibble.
#' @param caption Caption, which will be used with `knitr` call.
#'
#' @return Tibble or kable depending on the call.
#' @export
renderTibble <- function(tibble,
                         caption = NULL) {
    if (is.null(caption)) {
        stop("A caption is required.")
    }
    tibble <- tibble::as_tibble(tibble)
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
