#' @keywords internal
specialWords <- function(string) {
    string %>%
        # RNA classes:
        gsub("(m|nc|r)RNA", "\\1rna", .)
}
