#' Make names snake_case
#'
#' @author Michael Steinbaugh
#'
#' @param character Character vector
#'
#' @return Character vector formatted as snake_case
#' @export
#'
#' @examples
#' makeNamesSnake("RNAi clone")
makeNamesSnake <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.", "_", .)
}


#' @rdname makeNamesSnake
#' @export
snake <- makeNamesSnake
