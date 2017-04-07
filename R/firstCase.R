#' @rdname case
#' @export
#' @examples
#' firstCase("RNA sequencing")
firstCase <- function(character) {
    character %>%
        titleCase %>%
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE)
}
