#' Change case of a character vector
#'
#' @rdname case
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector
#'
#' @return Reformatted character vector



#' @rdname case
#' @description Title case
#' @export
#' @examples
#' titleCase("RNA sequencing")
titleCase <- function(character) {
    if (!is.character(character)) {
        stop("character vector required")
    }
    gsub("\\b([a-z])", "\\U\\1", character, perl = TRUE)
}



#' @rdname case
#' @description First case
#' @export
#' @examples
#' firstCase("RNA sequencing")
firstCase <- function(character) {
    character %>%
        titleCase %>%
        gsub("([A-Z])([A-Z]+)", "\\1\\L\\2", ., perl = TRUE)
}
