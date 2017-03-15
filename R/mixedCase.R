#' @keywords internal
mixedCase <- function(string) {
    string %>%
        gsub("\\b([a-z])", "\\U\\1", ., perl = TRUE) %>%
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE)
}
