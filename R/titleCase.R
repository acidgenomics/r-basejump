#' Change case of a character vector
#'
#' @rdname case
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector
#'
#' @return Reformatted character vector
#' @export
#'
#' @examples
#' titleCase("RNA sequencing")
titleCase <- function(character) {
    if (!is.character(character)) {
        stop("character vector required")
    }
    gsub("\\b([a-z])", "\\U\\1", character, perl = TRUE)
}
