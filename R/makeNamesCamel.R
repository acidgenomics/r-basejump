#' @rdname makeNames
#' @export
#' @examples
#' makeNamesCamel("RNAi clone")
makeNamesCamel <- function(character) {
    character %>%
        makeNames %>%
        gsub("\\.(\\w?)", "\\U\\1", ., perl = TRUE)
}



#' @rdname makeNames
#' @export
camel <- makeNamesCamel
