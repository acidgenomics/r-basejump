#' Source all R files in a directory
#'
#' @param path Defaults to `./R`.
#'
#' @export
sourcedir <- function(path = "R") {
  sapply(list.files(pattern = "[.]R$",
                    path = path,
                    full.names = TRUE), source)
}
