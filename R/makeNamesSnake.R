#' @rdname makeNames
#' @export
#' @examples
#' makeNamesSnake("RNAi clone")
makeNamesSnake <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.", "_", .)
}



#' @rdname makeNames
#' @export
snake <- makeNamesSnake
